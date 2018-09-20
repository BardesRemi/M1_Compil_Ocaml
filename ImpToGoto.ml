module Imp = ImpAST
module Gto = GotoAST
open CommonAST

let (++) = Gto.(++)
  
let new_label =
  let cpt = ref 0 in
  fun () -> incr cpt; CommonAST.Lab (Printf.sprintf "_label_%i" !cpt)

let rec translate_instruction = function
  | Imp.Sequence (i1, i2) -> Gto.Sequence(translate_instruction i1, translate_instruction i2)
  | Imp.Print e -> Gto.Print(translate_expression e)
  | Imp.Set (l, e) -> Gto.Set(translate_location l, translate_expression e)
  | Imp.Conditional (e, i1, i2) ->
     let then_label = new_label()
     and end_label = new_label()
     in
     Gto.ConditionalGoto(then_label, translate_expression e)
     ++ translate_instruction(i2) (* Code du bloc else *)
     ++ Gto.Goto(end_label) (* Fin du bloc then, aller fin *)
     ++ Gto.Label(then_label) (* Bloc then *)
     ++ translate_instruction(i1) (* Code du bloc then *)
     ++ Gto.Label(end_label)
       
  | Imp.Loop (e, i) ->
     let begin_label = new_label()
     and loop_label = new_label()
     and exit_label = new_label()
     in
     Gto.Label(begin_label) (* begin *)
     ++ Gto.ConditionalGoto(loop_label, translate_expression e)
     ++ Gto.Goto(exit_label) (* sortie de boucle *)
     ++ Gto.Label(loop_label) (* Bloc loop *)
     ++ translate_instruction(i) (* Code du bloc dans la loop *)
     ++ Gto.Goto(begin_label) (* on reboucle *)
     ++ Gto.Label(exit_label) (* on est sorti *)
  | Imp.Nop -> Gto.Nop
and translate_expression = function
  | Imp.Literal a -> Gto.Literal(a)
  | Imp.Location a -> Gto.Location(translate_location a)
  | Imp.UnaryOp (a, b) -> Gto.UnaryOp(a, translate_expression b)
  | Imp.BinaryOp (a, b, c) -> Gto.BinaryOp(a, translate_expression b, translate_expression c)
and translate_location = function
  | Imp.Identifier id -> Gto.Identifier(id)
     
let translate_program p = Gto.({
  main = translate_instruction Imp.(p.main);
  globals = Imp.(p.globals);
})
