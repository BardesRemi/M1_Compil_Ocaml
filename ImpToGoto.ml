module Imp = ImpAST
module Gto = GotoAST
open CommonAST

let (++) = Gto.(++)

exception Break_Continue_outside_loop
    
let new_label =
  let cpt = ref 0 in
  fun () -> incr cpt; CommonAST.Lab (Printf.sprintf "_label_%i" !cpt)

let rec translate_instruction i =
  match i with
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
     Gto.Label(begin_label) (* Début de la boucle avant évaluation de la condition *)
     ++ Gto.ConditionalGoto(loop_label, translate_expression e)
     ++ Gto.Goto(exit_label) (* Sortie de boucle *)
     ++ Gto.Label(loop_label) (* Début de l'intérieur de la boucle *)
     ++ translate_instruction_loop i begin_label exit_label (* Traduction du bloc de code dans la boucle *)
     ++ Gto.Goto(begin_label) (* On boucle *)
     ++ Gto.Label(exit_label) (* On est sortie *)
  | Imp.ForLoop (i_init, e_cond, i_incr, i) ->
     let begin_label = new_label()
     and loop_label = new_label()
     and exit_label = new_label()
     in
     translate_instruction i_init
     ++ Gto.Label(begin_label) (* Début de la boucle avant évaluation de la condition *)
     ++ Gto.ConditionalGoto(loop_label, translate_expression e_cond)
     ++ Gto.Goto(exit_label) (* Sortie de boucle *)
     ++ Gto.Label(loop_label) (* Début de l'intérieur de la boucle *)
     ++ translate_instruction_loop i begin_label exit_label (* Traduction du bloc de code dans la boucle *)
     ++ translate_instruction i_incr (* On incrémente, pas besoin de passer dans une gestion interne à la boucle *)
     ++ Gto.Goto(begin_label) (* On boucle *)
     ++ Gto.Label(exit_label) (* On est sortie *)
       
  | Imp.Break -> raise (Break_Continue_outside_loop) (* Pas de 'break' en dehors d'une boucle *)
  | Imp.Continue -> raise (Break_Continue_outside_loop) (* Pas de 'continue' en dehors d'une boucle *)
  | Imp.Nop -> Gto.Nop
(**
   Fonction de traduction des instructions dans une boucle.
   [translate_instruction_loop : ImpAST.instruction -> label -> label -> GtoAST.instruction]

   Utilisation d'une fonction auxiliaire possédant le label de début et de fin d'une boucle dans ses paramètres
   pour la gestion des instructions 'break' et 'continue'.
*)     
and translate_instruction_loop i b_label e_label =
  match i with
  | Imp.Break -> Gto.Goto(e_label)
  | Imp.Continue -> Gto.Goto(b_label)
  | Imp.Sequence (i1, i2) -> Gto.Sequence(translate_instruction_loop i1 b_label e_label,
					  translate_instruction_loop i2 b_label e_label)
  | Imp.Conditional (e, i1, i2) ->
     let then_label = new_label()
     and end_label = new_label()
     in
     Gto.ConditionalGoto(then_label, translate_expression e)
     ++ translate_instruction_loop i2 b_label e_label (* Code du bloc else *)
     ++ Gto.Goto(end_label) (* Fin du bloc then, aller fin *)
     ++ Gto.Label(then_label) (* Bloc then *)
     ++ translate_instruction_loop i1 b_label e_label (* Code du bloc then *)
     ++ Gto.Label(end_label)
  | _ -> translate_instruction i (* Toute instruction qui ne peut pas contenir de break/continue appartenant à cette boucle *)

and translate_expression = function
  | Imp.Literal l -> Gto.Literal(l)
  | Imp.Location l -> Gto.Location(translate_location l)
  | Imp.UnaryOp (op, Imp.Literal l) -> Gto.UnaryOp(op, Gto.Literal(l))
  | Imp.UnaryOp (op, e) -> Gto.UnaryOp(op, translate_expression e)
  | Imp.BinaryOp (op, Imp.Literal l1, Imp.Literal l2) -> Gto.BinaryOp(op, Gto.Literal l1, Gto.Literal l2) (* Gestion des expressions arithmétiques constantes de manière optimisé *)
  | Imp.BinaryOp (op, e1, e2) -> Gto.BinaryOp(op, translate_expression e1, translate_expression e2)
  | Imp.NewBlock(e) -> Gto.NewBlock(translate_expression e)
  | Imp.FunCall(id, e_list) -> Gto.FunCall(id, (List.map (fun x -> translate_expression x) e_list))
    
and translate_location = function
  | Imp.Identifier id -> Gto.Identifier(id)
  | Imp.BlockAccess (e1, e2) -> Gto.BlockAccess(translate_expression e1, translate_expression e2)
     
let translate_program p = Gto.({
  main = translate_instruction Imp.(p.main);
  globals = Imp.(p.globals);
  structs = Imp.(p.structs);
})
