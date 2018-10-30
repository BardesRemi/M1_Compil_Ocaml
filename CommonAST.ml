type identifier = Id  of string
type label      = Lab of string

module Symb_Tbl = Map.Make(String)
    
type typ =
  | TypInt
  | TypBool
  | TypArray of typ
  | TypStruct of string

type struct_type = {
  fields: (string * typ) list;
}
    
type type_context = {
  identifier_types: typ Symb_Tbl.t;
  struct_types: struct_type Symb_Tbl.t;
}

type literal =
  | Int  of int
  | Bool of bool

type unaryOp = Minus | Not
    
type binaryOp = Add | Sub | Mult | Div | Mod
                | Eq | Neq | Lt | Le | Gt | Ge
                | And | Or
