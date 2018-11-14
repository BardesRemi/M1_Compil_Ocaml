open CommonAST

type localised_expression = {
  expr : expression;
  e_pos : int * int;
}

and expression =
  | Literal   of literal
  | Location  of location
  | UnaryOp   of unaryOp  * localised_expression
  | BinaryOp  of binaryOp * localised_expression * localised_expression
  | NewArray  of localised_expression * typ
  | NewRecord of string
  | FunCall   of identifier * localised_expression list

and location =
  | Identifier  of identifier
  | ArrayAccess of localised_expression * localised_expression
  | FieldAccess of localised_expression * string

let mk_expr expr l c = { expr = expr; e_pos = l, c }

type localised_instruction = {
  instr : instruction;
  i_pos : int * int;
}

and instruction =
  | Print         of localised_expression
  | Set           of location * localised_expression
  | Conditional   of localised_expression * localised_instruction * localised_instruction
  | Loop          of localised_expression * localised_instruction
  | ForLoop       of localised_instruction * localised_expression * localised_instruction * localised_instruction
  | Sequence      of localised_instruction * localised_instruction
  | ProcedureCall of identifier * localised_expression list
  | Break
  | Continue
  | Return        of localised_expression
  | Nop

let mk_instr instr l c = { instr = instr; i_pos = l, c }

type function_info = {
  signature: function_signature;
  code: localised_instruction
}
  
type program = {
  main: localised_instruction;
  globals: typ Symb_Tbl.t;
  structs: struct_type Symb_Tbl.t;
  functions: function_info Symb_Tbl.t;
}
