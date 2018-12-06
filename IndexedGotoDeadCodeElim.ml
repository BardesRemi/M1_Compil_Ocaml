open IndexedGotoLiveness

let get_id l =
  let rec translate_expression = function
    | GotoAST.Location l -> translate_location l
    | GotoAST.Literal _
    | GotoAST.UnaryOp (_, _)  
    | GotoAST.BinaryOp (_, _, _)
    | GotoAST.NewBlock(_) 
    | GotoAST.FunCall(_, _) -> failwith "Never reached"
  and translate_location = function
    | GotoAST.Identifier (Id id) -> id
    | GotoAST.BlockAccess (e1, _) -> translate_expression e1
  in
  translate_location l
     
let step main_i =
  let li = liveness main_i in
  let delete = ref false in
  let rec step_rec i = 
    match i with
    | cpt, IndexedGotoAST.Set(l, e) ->
       if List.exists (fun x -> x = (get_id l)) li.live_out.(cpt)
       then i
       else
	 begin
	   delete := true;
	   (cpt, IndexedGotoAST.Nop);
	 end
    | cpt, IndexedGotoAST.Sequence(i1, i2) -> (cpt, Sequence(step_rec i1, step_rec i2))
    | cpt, IndexedGotoAST.Label(_)
    | cpt, IndexedGotoAST.Goto(_)
    | cpt, IndexedGotoAST.ConditionalGoto(_, _) 
    | cpt, IndexedGotoAST.Return(_)
    | cpt, IndexedGotoAST.ProcedureCall(_, _)
    | cpt, IndexedGotoAST.Nop -> i
  in
  (!delete, step_rec main_i)

let rec dead_code_elim i =
  let res = step i in
  match res with
  | (false, instr) -> instr
  | (true, instr) -> dead_code_elim instr
  
