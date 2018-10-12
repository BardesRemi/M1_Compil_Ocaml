module Src = SourceLocalisedAST
module Imp = ImpAST
open CommonAST

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
  | Src.Nop -> Imp.Nop
and strip_expression i = match Src.(i.expr) with
  | Src.Literal l -> Imp.Literal(l)
  | Src.Location loc -> Imp.Location(strip_location loc)
  | Src.UnaryOp (op, e) -> Imp.UnaryOp(op, strip_expression e)
  | Src.BinaryOp (op, e1, e2) -> Imp.BinaryOp(op, strip_expression e1, strip_expression e2)
  | Src.NewArray (e, t) -> Imp.NewBlock(strip_expression e)
and strip_location i = match i with
  | Src.Identifier id -> Imp.Identifier(id)
  | Src.ArrayAccess (e1, e2) -> Imp.BlockAccess(strip_expression e1, strip_expression e2)
      
let strip_program p =
  let main = strip_instruction Src.(p.main) in
  let globals = Src.(p.globals) in
  Imp.({ main; globals; })
