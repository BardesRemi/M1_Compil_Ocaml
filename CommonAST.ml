type identifier = Id  of string
type label      = Lab of string

module Symb_Tbl = Map.Make(String)
    
type typ =
  | TypInt
  | TypBool
  | TypArray of typ
  | TypStruct of string
  | TypVoid

type struct_type = {
  fields: (string * typ * bool) list;
}

type function_signature = {
  return: typ;
  formals: (string * typ) list;
}
    
type type_context = {
  identifier_types: typ Symb_Tbl.t;
  struct_types: struct_type Symb_Tbl.t;
  function_signatures: function_signature Symb_Tbl.t;
  return_type: typ;
}

type literal =
  | Int  of int
  | Bool of bool

type unaryOp = Minus | Not
    
type binaryOp = Add | Sub | Mult | Div | Mod
                | Eq | Neq | Lt | Le | Gt | Ge
                | And | Or

let transform_name prev_name params =
    let rec typ_to_string t = match t with
      | TypInt -> "int"
      | TypBool -> "bool"
      | TypArray(ty) -> "array_of_"^(typ_to_string ty)
      | TypStruct(name) -> "struct_"^name
      | TypVoid -> failwith "TypVoid forbidden as function parameter"
    in
    List.fold_left (fun acc t -> acc^"_"^(typ_to_string t)) prev_name params 
