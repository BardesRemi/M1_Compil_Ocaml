open CommonAST
open SourceLocalisedAST

exception Type_error of typ * typ * (int * int)

let rec find_field l field_name =
  match l with
  | [] -> failwith (Printf.sprintf "Unknown field")
  | (name, t, imm)::n -> if field_name = name then (name, t, imm) else find_field n field_name

let type_literal = function
  | Int _ -> TypInt
  | Bool _ -> TypBool
     
let rec check_type context pos e ty =
  let t = type_expression context e in
  if t = ty
  then ()
  else raise (Type_error(ty, t, pos))
    
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
	 let (_, t, _) = (List.find (fun (n,_,_) -> n = name) s.fields) in t
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
  | FunCall(Id funName, e_list) ->
     begin
       let signature = Symb_Tbl.find funName context.function_signatures in
       let return_typ = signature.return in
       let rec checking_elist_type elist tlist =
	 match elist, tlist with
	 | [], []   -> return_typ
	 | expr::l1, t::l2 -> check_type context e.e_pos expr (snd t); checking_elist_type l1 l2
	 | _, _ -> failwith (Printf.sprintf "Unmatched numbers of arguments : (%d, %d)" (fst e.e_pos) (snd e.e_pos))
       in
       checking_elist_type e_list signature.formals
     end
									
let rec typecheck_instruction context i = match i.instr with
  | Set (l, expr) ->
     begin
	 match l with
	 | FieldAccess(e, f) ->
	    let typ = type_expression context e in
	    begin
	      match typ with
	      | TypStruct(name) -> let structure = Symb_Tbl.find name context.struct_types in
				   let (_, _, immutable)= find_field structure.fields f in
				   if immutable
				   then failwith (Printf.sprintf "Field immutable : (%d, %d)" (fst i.i_pos) (snd i.i_pos))
				   else check_type context i.i_pos expr (type_location context l)
	      | _ -> failwith (Printf.sprintf "Type Struct expected at position : (%d, %d)" (fst i.i_pos) (snd i.i_pos))
	    end

	 | _ -> check_type context i.i_pos expr (type_location context l)
     end
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
  | Return(e) -> check_type context i.i_pos e context.return_type
  | ProcedureCall(Id funName, e_list) ->
     begin
       let signature = Symb_Tbl.find funName context.function_signatures in
       let rec checking_elist_type elist tlist =
	 match elist, tlist with
	 | [], [] -> ()
	 | expr::l1, t::l2 -> check_type context i.i_pos expr (snd t); checking_elist_type l1 l2
	 | _, _ -> failwith (Printf.sprintf "Unmatched numbers of arguments : (%d, %d)" (fst i.i_pos) (snd i.i_pos))
       in
       checking_elist_type e_list signature.formals
     end
  | Nop -> ()
   

let extract_context p idents f ty = { identifier_types = idents;
				      struct_types = p.structs;
				      function_signatures = f;
				      return_type = ty }
    
let typecheck_program p =
  let functions = Symb_Tbl.fold (fun k f acc -> Symb_Tbl.add k f.signature acc) p.functions Symb_Tbl.empty in 
  let predefined_signatures =
    Symb_Tbl.add "print_int" { return=TypInt; formals=["x", TypInt] }
      (Symb_Tbl.add "power" { return=TypInt; formals=["x", TypInt; "n", TypInt] }
	 (Symb_Tbl.add "print" { return=TypVoid; formals=["x", TypInt] } functions)) in
      
  let type_context = extract_context p p.globals predefined_signatures TypVoid in
  typecheck_instruction type_context p.main;

  let check_functions k f =
    let temp_vars_table = List.fold_left (fun acc x -> Symb_Tbl.add (fst x) (snd x) acc) p.globals f.signature.formals in
    let type_context = extract_context p (Symb_Tbl.fold (fun k v acc -> Symb_Tbl.add k v acc) f.locals temp_vars_table) predefined_signatures f.signature.return in
    
    typecheck_instruction type_context f.code
  in
  Symb_Tbl.iter (check_functions) p.functions;
  type_context;
