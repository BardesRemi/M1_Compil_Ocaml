open CommonAST

type expression =
  | Literal  of literal
  | Location of location
  | UnaryOp  of unaryOp  * expression
  | BinaryOp of binaryOp * expression * expression
  | NewBlock of expression
  | FunCall  of identifier * expression list

and location =
  | Identifier  of identifier
  | BlockAccess of expression * expression
  
type instruction =
  | Sequence        of instruction * instruction
  | Print           of expression
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
