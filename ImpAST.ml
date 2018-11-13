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
  | Print       of expression
  | Set         of location   * expression
  | Conditional of expression * instruction * instruction
  | Loop        of expression * instruction
  | ForLoop     of instruction * expression * instruction * instruction
  | Sequence    of instruction * instruction
  | Break
  | Continue
  | Return      of expression
  | Nop

type function_info = {
  signature: function_signature;
  code: instruction
}
  
type program = {
  main: instruction;
  globals: typ Symb_Tbl.t;
  structs: struct_type Symb_Tbl.t;
  functions: function_info Symb_Tbl.t;
}
