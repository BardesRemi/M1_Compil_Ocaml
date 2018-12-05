open CommonAST

type expression = GotoAST.expression
type location = GotoAST.location

type instruction = int * instr_descr
and instr_descr =
  | Sequence        of instruction * instruction
  | Set             of location * expression
  | Label           of label
  | Goto            of label
  | ConditionalGoto of label * expression
  | Return          of expression
  | ProcedureCall   of identifier * expression list
  | Nop

let (++) i1 i2 = Sequence(i1, i2)

type function_info = {
  locals: typ Symb_Tbl.t;
  signature: function_signature;
  code: instruction
}
  
type program = {
  main: instruction;
  globals: typ Symb_Tbl.t;
  structs: struct_type Symb_Tbl.t;
  functions: function_info Symb_Tbl.t;
}

let index_bloc i =
  let cpt = ref (-1) in
  let rec index_instruction_rec i = 
    cpt := !cpt + 1;
    let c = !cpt in
    match i with
    | GotoAST.Sequence(i1, i2) -> (c, Sequence(index_instruction_rec i1, index_instruction_rec i2))
    | GotoAST.Set(l, e) -> (c, Set(l, e))
    | GotoAST.Label l -> (c, Label(l))
    | GotoAST.Goto l -> (c, Goto(l))
    | GotoAST.ConditionalGoto(l, e) -> (c, ConditionalGoto(l, e))
    | GotoAST.Return e -> (c, Return(e))
    | GotoAST.ProcedureCall(id, e_list) -> (c, ProcedureCall(id, e_list))
    | GotoAST.Nop -> (c, Nop)
  in
  index_instruction_rec i

let rec strip_instruction : instruction -> GotoAST.instruction = function
  | _, Sequence(i1, i2) -> GotoAST.Sequence(strip_instruction i1, strip_instruction i2)
  | _, Set(l, e) -> GotoAST.Set(l, e)
  | _, Label(l) -> GotoAST.Label(l)
  | _, Goto(l) -> GotoAST.Goto(l)
  | _, ConditionalGoto(l, e) -> GotoAST.ConditionalGoto(l, e)
  | _, Return(e) -> GotoAST.Return(e)
  | _, ProcedureCall(id, e_list) -> GotoAST.ProcedureCall(id, e_list)
  | _, Nop -> GotoAST.Nop  

let index_program p =
  { main = (index_bloc GotoAST.(p.main));
    globals = GotoAST.(p.globals);
    structs = GotoAST.(p.structs);
    functions = Symb_Tbl.fold (fun k f acc -> Symb_Tbl.add k { locals = GotoAST.(f.locals);
							       signature = GotoAST.(f.signature);
							       code = (index_bloc GotoAST.(f.code)) } acc) GotoAST.(p.functions) Symb_Tbl.empty }
    
let strip_program p = 
  { GotoAST.main = (strip_instruction p.main);
    globals = p.globals;
    structs = p.structs;
    GotoAST.functions = Symb_Tbl.fold (fun k f acc -> Symb_Tbl.add k { locals = f.locals;
								       signature = f.signature;
								       GotoAST.code = (strip_instruction f.code) } acc) p.functions Symb_Tbl.empty }
