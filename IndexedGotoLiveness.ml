module IndexedGoto = IndexedGotoAST

type succ_table = int list array
  
type liveness_info = { live_in: string list array;
		       live_out: string list array }

let rec instr_size i array_size =
    match i with
    | n, IndexedGoto.Sequence(i1, i2) ->
       array_size := !array_size + 1;
       instr_size i1;
       instr_size i2
    | n, _ -> array_size := !array_size + 1
in

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
       if num + 1 > (!array_size - 1) then
	 begin
	   get_label_succ label main_i;
	   [(!label_succ)]
	 end
       else
	 begin
	   get_label_succ label main_i;
	   (!label_succ)::[num + 1]
	 end
    | _ -> if num + 1 > (!array_size - 1) then [] else [num + 1]
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
  let li = { live_in = Array.make size [];
	 live_out = Array.make size [] }
  in
  let rec liveness_rec li i = ...
      begin
	
      end
  in
  if li = @res liveness_rec li i then li else
  liveness_rec li main_i
  

