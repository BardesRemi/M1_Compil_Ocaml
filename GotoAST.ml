open CommonAST

type expression =
  | Literal  of literal
  | Location of location
  | UnaryOp  of unaryOp  * expression
  | BinaryOp of binaryOp * expression * expression
  | NewBlock of expression

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
  | Nop

let (++) i1 i2 = Sequence(i1, i2)

type program = {
  main: instruction;
  globals: typ Symb_Tbl.t;
}
