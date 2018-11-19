module Src = SourceLocalisedAST
module Imp = ImpAST
open CommonAST
open SourceTypeChecker

(* Donne la position du champ f dans la liste de champ l *)
let find_pos l f =
  let rec find_pos_rec l f c =
    match l with
    | [] -> failwith (Printf.sprintf "Unknown field")
    | (name, t, imm)::n -> if name = f then c else find_pos_rec n f (c+1)
  in
  find_pos_rec l f 0

let strip_instruction_main type_context i =
  let rec strip_instruction i = match Src.(i.instr) with
    | Src.Print e -> Imp.Print(strip_expression e)
    | Src.Set (loc, e)-> Imp.Set(strip_location loc, strip_expression e)
    | Src.Conditional (e, i1, i2) -> Imp.Conditional(strip_expression e, strip_instruction i1, strip_instruction i2) 
    | Src.Loop (e, i) -> Imp.Loop(strip_expression e, strip_instruction i)
    | Src.ForLoop (i_init, e_cond, i_incr, i) ->
       Imp.ForLoop(strip_instruction i_init, strip_expression e_cond, strip_instruction i_incr, strip_instruction i)
    | Src.Sequence (i1, i2) -> Imp.Sequence(strip_instruction i1, strip_instruction i2)
    | Src.Break -> Imp.Break
    | Src.Continue -> Imp.Continue
    | Src.Return(e) -> Imp.Return(strip_expression e)
    | Src.ProcedureCall(id, e_list) -> Imp.ProcedureCall(id, (List.map (fun x -> strip_expression x) e_list))
    | Src.Nop -> Imp.Nop
  and strip_expression i = match Src.(i.expr) with
    | Src.Literal l -> Imp.Literal(l)
    | Src.Location loc -> Imp.Location(strip_location loc)
    | Src.UnaryOp (op, e) -> Imp.UnaryOp(op, strip_expression e)
    | Src.BinaryOp (op, e1, e2) -> Imp.BinaryOp(op, strip_expression e1, strip_expression e2)
    | Src.NewArray (e, t) -> Imp.NewBlock(strip_expression e)
    | Src.NewRecord(name) -> 
       let name_type = Symb_Tbl.find name type_context.struct_types in
       let size = List.length name_type.fields in
       Imp.NewBlock(Imp.Literal(Int(size)))
    | Src.FunCall(id, e_list) -> Imp.FunCall(id, (List.map (fun x -> strip_expression x) e_list))
  and strip_location i = match i with
    | Src.Identifier id -> Imp.Identifier(id)
    | Src.ArrayAccess (e1, e2) -> Imp.BlockAccess(strip_expression e1, strip_expression e2)
    | Src.FieldAccess(e, f) ->
       let typ = type_expression type_context e in
       begin
	 match typ with
	 | TypStruct(name) -> let structure = Symb_Tbl.find name type_context.struct_types in
			      let pos = find_pos structure.fields f in
			      Imp.BlockAccess(strip_expression e, Imp.Literal(Int(pos)))
	 | _ -> failwith (Printf.sprintf "Type Struct expected")
       end
  in
  strip_instruction i
      
let strip_program p type_context =
  let main = strip_instruction_main type_context Src.(p.main) in
  let globals = Src.(p.globals) in
  let structs = Src.(p.structs) in
  let functions = Symb_Tbl.fold (fun k f acc -> Symb_Tbl.add k Imp.({locals=Src.(f.locals);
								     signature=Src.(f.signature);
							             code=strip_instruction_main type_context Src.(f.code)}) acc) Src.(p.functions) (Symb_Tbl.empty) in
  Imp.({ main; globals; structs; functions })
