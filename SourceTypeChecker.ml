open CommonAST
open SourceLocalisedAST

exception Type_error of typ * typ * (int * int)
    
let type_literal = function
  | Int _ -> TypInt
  | Bool _ -> TypBool
     
let rec check_type context pos e ty =
  let t = type_expression context e in
  if t = ty
  then ()
  else begin
    if t = TypInt then failwith (Printf.sprintf "TypInt")
    else if t = TypBool then failwith (Printf.sprintf "TypBool")
    else failwith (Printf.sprintf "TypArray/TypStruct") end (* Printf.printf "test"; raise (Type_error(ty, t, pos)) end *)
    
and type_location context l = match l with
  | Identifier(Id name) -> Symb_Tbl.find name context.identifier_types
  | ArrayAccess(e1, e2) ->
     begin
       match (type_expression context e1) with
       | TypArray(ty)     -> check_type context e1.e_pos e2 TypInt; ty
       | _                -> failwith (Printf.sprintf "Type Array expected at position : (%d, %d)" (fst e1.e_pos) (snd e1.e_pos))
     end
  | FieldAccess(struct_id, name) ->
     begin
       let s =
	 match (type_expression context struct_id) with
	 | TypStruct(name)  -> Symb_Tbl.find name context.struct_types
	 | _                -> failwith (Printf.sprintf "Type Struct expected at position : (%d, %d)" (fst struct_id.e_pos) (snd struct_id.e_pos))
       in
       try
	 let field = (List.find (fun x -> (fst x) = name) s.fields) in
	 snd field
       with
       | Not_found -> failwith (Printf.sprintf "Unknown field at position : (%d, %d)" (fst struct_id.e_pos) (snd struct_id.e_pos))
     end
       
and type_expression context e = match e.expr with
  | Literal l -> type_literal l
  | Location loc -> type_location context loc
  | UnaryOp (Minus, b) -> check_type context e.e_pos b TypInt; TypInt
  | UnaryOp (Not, b) ->  check_type context e.e_pos b TypBool; TypBool
  | BinaryOp (Add, b, c)
  | BinaryOp (Sub, b, c)
  | BinaryOp (Mult, b, c)
  | BinaryOp (Div, b, c)
  | BinaryOp (Mod, b, c) -> check_type context e.e_pos c TypInt; TypInt
  | BinaryOp (Eq, b, c) | BinaryOp (Neq, b, c) -> let t1 = type_expression context b in
						  let t2 = type_expression context c in
						  if t1 = t2 then TypBool else raise (Type_error(t1, t2, e.e_pos))
  | BinaryOp (Lt, b, c)
  | BinaryOp (Le, b, c)
  | BinaryOp (Gt, b, c)
  | BinaryOp (Ge, b, c) -> check_type context e.e_pos b TypInt; check_type context e.e_pos c TypInt; TypBool
  | BinaryOp (And, b, c) | BinaryOp (Or, b, c) -> check_type context e.e_pos b TypBool; check_type context e.e_pos c TypBool; TypBool
  | NewArray(e_bis, ty) -> check_type context e.e_pos e_bis TypInt; TypArray(ty)
  | NewRecord (name) -> let _ = Symb_Tbl.find name context.struct_types in TypStruct(name)
									
let rec typecheck_instruction context i = match i.instr with
  | Print e -> check_type context i.i_pos e TypInt
  | Set (l, e) -> check_type context i.i_pos e (type_location context l)
  | Conditional (e, i1, i2) ->
    check_type context i.i_pos e TypBool;
    typecheck_instruction context i1;
    typecheck_instruction context i2
  | Loop (e, i) ->
    check_type context i.i_pos e TypBool;
    typecheck_instruction context i;
  | ForLoop (i_init, e_cond, i_incr, i) ->
    typecheck_instruction context i_init;
    check_type context i.i_pos e_cond TypBool;
    typecheck_instruction context i_incr;
    typecheck_instruction context i
  | Sequence (i1, i2) ->
    typecheck_instruction context i1;
    typecheck_instruction context i2
  | Break -> ()
  | Continue -> ()
  | Nop -> ()
   

let extract_context p =
  { identifier_types = p.globals; struct_types = p.structs }
    
let typecheck_program p =
  let type_context = extract_context p in
  typecheck_instruction type_context p.main;
  type_context;
