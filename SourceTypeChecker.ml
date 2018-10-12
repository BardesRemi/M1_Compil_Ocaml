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
  else raise (Type_error(ty, t, pos))
and type_expression context e = match e.expr with
  | Literal l -> type_literal l
  | Location(Identifier(Id name)) -> Symb_Tbl.find name context.identifier_types
  | Location (BlockAccess(e1, e2)) ->
     begin
       match (type_expression e1) with
       | TypInt | TypBool -> failwith (Printf.sprintf "Type Array expected at position : (%d, %d)" (fst e.e_pos) (snd e.e_pos)) 
       | TypArray(ty)     -> check_type context e.e_pos e2 TypInt; ty
     end
  | UnaryOp (Minus, b) -> check_type context e.e_pos b TypInt; TypInt
  | UnaryOp (Not, b) ->  check_type context e.e_pos b TypBool; TypBool
  | BinaryOp (Add, b, c)
  | BinaryOp (Sub, b, c)
  | BinaryOp (Mult, b, c)
  | BinaryOp (Div, b, c)
  | BinaryOp (Mod, b, c) -> check_type context e.e_pos b TypInt; check_type context e.e_pos c TypInt; TypInt
  | BinaryOp (Eq, b, c) | BinaryOp (Neq, b, c) -> let t1 = type_expression context b in
						  let t2 = type_expression context c in
						  if t1 = t2 then TypBool else raise (Type_error(t1, t2, e.e_pos)) 
  | BinaryOp (Lt, b, c)
  | BinaryOp (Le, b, c)
  | BinaryOp (Gt, b, c)
  | BinaryOp (Ge, b, c) -> check_type context e.e_pos b TypInt; check_type context e.e_pos c TypInt; TypBool
  | BinaryOp (And, b, c) | BinaryOp (Or, b, c) -> check_type context e.e_pos b TypBool; check_type context e.e_pos c TypBool; TypBool
  | NewArray(e_bis, ty) -> check_type context e.e_pos e_bis TypInt; TypArray(ty)
    
let rec typecheck_instruction context i = match i.instr with
  | Print e -> check_type context i.i_pos e TypInt
  | Set (Identifier(Id name), e) -> check_type context i.i_pos e (Symb_Tbl.find name context.identifier_types)
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
  { identifier_types = p.globals; }
    
let typecheck_program p =
  let type_context = extract_context p in
  typecheck_instruction type_context p.main;
