module IndexedGoto = IndexedGotoAST

type succ_table = int list array
  
type liveness_info = { live_in: string list array;
		       live_out: string list array }

type killGenDouble = { kill_list : string list;
		       gen_list : string list }

let rec instr_size i array_size =
    match i with
    | n, IndexedGoto.Sequence(i1, i2) ->
       array_size := !array_size + 1;
       instr_size i1 array_size;
       instr_size i2 array_size 
    | n, _ -> array_size := !array_size + 1

let mk_succ_table main_i =
  let label_succ = ref (-1) in
  let array_size = ref 0 in
  instr_size main_i array_size;
  let array = Array.make !array_size [] in
  let rec get_label_succ label i =
    match i with
    | _, IndexedGoto.Sequence(i1, i2) -> get_label_succ label i1; get_label_succ label i2
    | n, IndexedGoto.Label(l) -> if l = label then label_succ := n else ()
    | _ -> ()
  in
  let get_succ num i_descr =
    match i_descr with
    | IndexedGoto.Goto(label) -> get_label_succ label main_i; [(!label_succ)]
    | IndexedGoto.ConditionalGoto(label, _) ->
       if num - 1 < 0 then
	 begin
	   get_label_succ label main_i;
	   [(!label_succ)]
	 end
       else
	 begin
	   get_label_succ label main_i;
	   (!label_succ)::[num - 1]
	 end
    | _ -> if num - 1 < 0 then [] else [num - 1]
  in
  let rec mk_succ_table_rec i =
    match i with
    | n, (IndexedGoto.Sequence(i1, i2) as i_descr) ->
      let succ = get_succ n i_descr in
      Array.set array n succ;
      mk_succ_table_rec i1;
      mk_succ_table_rec i2 
    | n, i_descr ->
      let succ = get_succ n i_descr in
      Array.set array n succ;
  in
  mk_succ_table_rec main_i;
  array
    
let liveness main_i =
  let size = ref 0 in
  instr_size main_i size;
  let succs = mk_succ_table main_i in
  (* Concernant ces deux fonctions mutuellement recursives, la liste résultat généré peux contenir des 
     doublons. Ceux-ci augmenterons le temps de recherche future dans le tableau. Cependant on pense qu'il 
     serait plus couteux de supprimer les doublons éventuellement généré que de ne rien faire et légèrement 
     augmenter le temps de recherche dans le tableaux. *)
  let rec get_expr_labs expr res =
    match expr with
    |GotoAST.Literal(n)           -> res
    |GotoAST.Location(loc)        -> get_location_labs loc res
    |GotoAST.UnaryOp(op, e)       -> get_expr_labs e res
    |GotoAST.BinaryOp(op, e1, e2) -> get_expr_labs e1 (get_expr_labs e2 res)
    |GotoAST.NewBlock(e)          -> get_expr_labs e res
    |GotoAST.FunCall (id, e_list) -> List.fold_left (fun acc x -> (get_expr_labs x acc)) res e_list
       
  and get_location_labs loc res =
    match loc with
    |GotoAST.Identifier(Id id)      -> id::res
    |GotoAST.BlockAccess (e1,e2)    -> get_expr_labs e1 (get_expr_labs e2 res)
  in
  
  let killGen_tbl = Array.make !size { kill_list=[]; gen_list=[] } in
  let rec killGen_generator instr =
    match instr with
    | cpt, IndexedGoto.Sequence(i1, i2)          ->
       killGen_generator i1; killGen_generator i2
    | cpt, IndexedGoto.Set(l, e)                 ->
       Array.set killGen_tbl cpt {kill_list= get_location_labs l []; gen_list= get_expr_labs e []}
    | cpt, IndexedGoto.Label(_)
    | cpt, IndexedGoto.Nop
    | cpt, IndexedGoto.Goto(_)                   ->
       ()
    | cpt, IndexedGoto.ConditionalGoto(l, e)     ->
       Array.set killGen_tbl cpt { kill_list= []; gen_list= get_expr_labs e [] }
    | cpt, IndexedGoto.Return(e)                 ->
       Array.set killGen_tbl cpt { kill_list= []; gen_list= get_expr_labs e [] }
    | cpt, IndexedGoto.ProcedureCall(id, e_list) ->
       Array.set killGen_tbl cpt { kill_list= []; gen_list= List.fold_left (fun acc x -> get_expr_labs x acc) [] e_list }
  in
  killGen_generator main_i;
  
  let li = { live_in = Array.make !size [];
	     live_out = Array.make !size [] }
  in

  (* Toujours la meme histoire concernant les duplicata *)
  let rec get_out succs res=
    match succs with
    |[]   -> res
    |x::s -> get_out s (List.append (Array.get li.live_in x) res)
  in
  
  let rec get_in cpt out =
    (* Out[p] \ Kill[p] *)
    let t1 = List.filter (fun x -> if (List.exists (fun v -> if x = v then true else false) killGen_tbl.(cpt).kill_list) then false else true ) out in
    (* t1 U Gen[p] *)
    List.append t1 killGen_tbl.(cpt).gen_list
  in
  (* Test l'égalité entre deux liveness_info *)
  let equal_li li1 li2 =
    let res = ref true in
    let i = ref 0 in
    while (((!i)<(!size)) && (!res)) do
      begin
	let li1_in = List.sort_uniq compare (li1.live_in.(!i)) in
	let li2_in = List.sort_uniq compare (li2.live_in.(!i)) in
	if li1_in = li2_in then () else res := false;
	let li1_out = List.sort_uniq compare (li1.live_out.(!i)) in
	let li2_out = List.sort_uniq compare (li2.live_out.(!i)) in
	if li1_out = li2_out then () else res := false;
	incr i;
      end
    done;
    !res
  in
  
  let rec liveness_rec i =
    let old_li = { live_in = Array.copy li.live_in;
		   live_out = Array.copy li.live_out } in
    let rec one_turn_li instr =
      match instr with
      | cpt, IndexedGoto.Sequence((n1, i1), (n2, i2))          ->
	 begin
	   one_turn_li (n1, i1);
	   one_turn_li (n2, i2);
	   Array.set li.live_in cpt li.live_in.(n1);
	   Array.set li.live_out cpt li.live_out.(n2)
	 end
      | cpt, IndexedGoto.Set(_, _)
      | cpt, IndexedGoto.Label(_)
      | cpt, IndexedGoto.Nop
      | cpt, IndexedGoto.Goto(_)
      | cpt, IndexedGoto.ConditionalGoto(_, _)
      | cpt, IndexedGoto.Return(_)
      | cpt, IndexedGoto.ProcedureCall(_, _) ->
	 begin
	   Array.set li.live_out cpt (get_out (Array.get succs cpt) []);
	   Array.set li.live_in cpt (get_in cpt (Array.get li.live_out cpt))
	 end
    in
    one_turn_li i;
    if equal_li old_li li then () else liveness_rec main_i
  in
  liveness_rec main_i;
  li
