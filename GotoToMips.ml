open CommonAST
open GotoAST
open Mips

(* Fonctions auxiliaires fournissant les pseudo-instructions [push] et [pop]. *)
let push reg = sw reg 0 sp  @@ subi sp sp 4
let pop  reg = addi sp sp 4 @@ lw reg 0 sp

(**
   Fonction de traduction des expressions.
   [translate_expression : GotoAST.expression -> Mips.text]

   Rappel de la convention : le code généré par [translate_expression e] doit
   placer la valeur de l'expression [e] au sommet de la pile.

   Les fonctions translates sont optimisées de façon à éviter de dépiler immédiatement un élément qui
   vient d'etre empilé.
   Dans le cadre de cette optimisation, la gestion des opérateur binaire (a op b) se fait de la façon suivante :
       - on traduit 'a' (via appel récursif) qui se retrouve dans t0
       - on voudrait lire 'b', cependant cela écraserai le contenu de t0
       - on stock donc 'a' sur le dessus de la pile (push t0)
       - on traduit 'b' (appel récursif) qui se retrouve dans t0
       - on récupère 'a' dans un autre registre que t0 (pop t1)
       - on effectue l'opération t1 op t0 (dans cette ordre pour respécter les opérations non commutatives)
*)
let rec translate_expression (e: GotoAST.expression) = match e with
  | GotoAST.Literal (Int i) ->
     li t0 i
  | GotoAST.Literal (Bool b) ->
     begin
     match b with
     | true ->
	li t0 (-1)
     | false ->
	li t0 0
     end
       
  | GotoAST.Location (Identifier(Id name)) ->
     la t0 name
     @@ lw t0 0(t0)
  | GotoAST.Location (BlockAccess(e1, e2)) ->
     translate_expression e1
     @@ push t0
     @@ translate_expression e2
     @@ pop t1
     @@ li t2 4
     @@ mul t0 t0 t2
     @@ add t1 t1 t0
     @@ lw t0 0(t1)
  | GotoAST.UnaryOp (Minus, e) ->
     translate_expression e
     @@ neg t0 t0
  | GotoAST.UnaryOp (Not, e) ->
     translate_expression e
     @@ not_ t0 t0
  | GotoAST.BinaryOp (Add, Literal (Int i), e) ->
     translate_expression e
     @@ addi t0 t0 i
  | GotoAST.BinaryOp (Add, e, Literal (Int i)) ->
     translate_expression e
     @@ addi t0 t0 i
  | GotoAST.BinaryOp (Add, e1, e2) ->
     translate_expression e1
     @@ push t0
     @@ translate_expression e2
     @@ pop t1
     @@ add t0 t1 t0
  | GotoAST.BinaryOp (Sub, e1, e2) ->
     translate_expression e1
     @@ push t0
     @@ translate_expression e2
     @@ pop t1
     @@ sub t0 t1 t0
  | GotoAST.BinaryOp (Mult, e1, e2) ->
     translate_expression e1
     @@ push t0
     @@ translate_expression e2
     @@ pop t1
     @@ mul t0 t1 t0
  | GotoAST.BinaryOp (Div, e1, e2) ->
     translate_expression e1
     @@ push t0
     @@ translate_expression e2
     @@ pop t1
     @@ div t0 t1 t0
  | GotoAST.BinaryOp (Mod, e1, e2) ->
     translate_expression e1
     @@ push t0
     @@ translate_expression e2
     @@ pop t1
     @@ rem t0 t1 t0
  | GotoAST.BinaryOp (Eq, e1, e2) ->
     translate_expression e1
     @@ push t0
     @@ translate_expression e2
     @@ pop t1
     @@ seq t0 t1 t0
  | GotoAST.BinaryOp (Neq, e1, e2) ->
     translate_expression e1
     @@ push t0
     @@ translate_expression e2
     @@ pop t1
     @@ sne t0 t1 t0
  | GotoAST.BinaryOp (Lt, e1, e2) ->
     translate_expression e1
     @@ push t0
     @@ translate_expression e2
     @@ pop t1
     @@ slt t0 t1 t0
  | GotoAST.BinaryOp (Le, e1, e2) ->
     translate_expression e1
     @@ push t0
     @@ translate_expression e2
     @@ pop t1
     @@ sle t0 t1 t0
  | GotoAST.BinaryOp (Gt, e1, e2) ->
     translate_expression e1
     @@ push t0
     @@ translate_expression e2
     @@ pop t1
     @@ sgt t0 t1 t0
  | GotoAST.BinaryOp (Ge, e1, e2) ->
     translate_expression e1
     @@ push t0
     @@ translate_expression e2
     @@ pop t1
     @@ sge t0 t1 t0
  | GotoAST.BinaryOp (And, e1, e2) ->
     translate_expression e1
     @@ push t0
     @@ translate_expression e2
     @@ pop t1
     @@ and_ t0 t1 t0
  | GotoAST.BinaryOp (Or, e1, e2) ->
     translate_expression e1
     @@ push t0
     @@ translate_expression e2
     @@ pop t1
     @@ or_ t0 t1 t0
  | GotoAST.NewBlock(e) ->
     translate_expression e
     @@ addi t1 t0 1
     @@ move a0 t1
     @@ li t1 4
     @@ mul a0 a0 t1
     @@ li v0 9
     @@ syscall
     @@ sw t0 0(v0)
     @@ addi t0 v0 4 (* $t0 <- adresse du premier champ *)

(**
   Fonction de traduction des locations.
   [translate_location : GotoAST.location -> Mips.text]
*)
and translate_location = function
| GotoAST.Identifier(Id name) ->
     la t0 name
| GotoAST.BlockAccess(e1, e2) ->
   translate_expression e1
   @@ push t0
   @@ translate_expression e2
   @@ pop t1
   @@ li t2 4
   @@ mul t0 t0 t2 
   @@ add t0 t1 t0
     
       
(**
   Fonction de traduction des instructions.
   [translate_instruction : GotoAST.instruction -> Mips.text]
*)
let rec translate_instruction (i: GotoAST.instruction) = match i with
  | GotoAST.Sequence (i1, i2) ->
     translate_instruction i1
     @@ translate_instruction i2
  | GotoAST.Print e ->
     translate_expression e
     @@ move a0 t0
     @@ li v0 1
     @@ syscall
  | GotoAST.Set (l, e) ->
     translate_expression e
     @@ push t0
     @@ translate_location l
     @@ pop t1
     @@ sw t1 0(t0)
  | GotoAST.Label(Lab l) -> label l
  | GotoAST.Goto (Lab l) -> b l
  | GotoAST.ConditionalGoto (Lab l, e) ->
     translate_expression e
     @@ bne zero t0 l
  | GotoAST.Nop -> nop


(** 
    Fonction de traduction des programmes
    [translate_program : GotoAST.program -> Mips.program]

    Rien à changer dans cette fonction, elle fournit déjà l'infrastructure dans
    laquelle insérer le code principal.
*)
let translate_program program =
  (* Initialisation : lit le paramètre donné en entrée et enregistre le résultat
     dans les données statiques sous l'étiquette [arg].
     À défaut de paramètre, [arg] vaudra zéro. *)
  let init =
    beqz a0 "init_end"
    @@ lw a0 0 a1
    @@ jal "atoi"
    @@ la t0 "arg"
    @@ sw v0 0 t0
    @@ label "init_end"
      
  (* Terminaison du programme avec l'appel système [exit] *)
  and close =
    li v0 10
    @@ syscall

  (* Fonctions prédéfinies.
     En l'occurrence, fonction de lecture du paramètre d'entrée. *)
  and built_ins =
    (* Le paramètre est donné sous la forme d'une chaîne de caractères
       terminée par le caractère [000]. *)
    label "atoi"
      
    (* Variables *)
    @@ move t0 a0 (* t0 : adresse du caractère à lire *)
    @@ li   t1 0  (* t1 : accumulateur pour la valeur calculée *)
    (* On garde t2 pour des calculs intermédiaires *)
      
    (* Constantes *)
    @@ li   t3 10 (* Base décimale *)
    @@ li   t4 48 (* Code ASCII caractère '0' *)
    @@ li   t5 57 (* Code ASCII caractère '9' *)

    (* Début de la boucle de lecture *)
    @@ label "atoi_loop"
    @@ lbu  t2 0 t0 (* Lecture d'un octet *)

    (* Conditions d'arrêt et d'erreur *)
    @@ beq  t2 zero "atoi_end" (* Fin si lecture de [000] *)
    @@ blt  t2 t4 "atoi_error" (* Erreur si caractère non compris entre 0 et 9 *)
    @@ bgt  t2 t5 "atoi_error"

    (* Mise à jour de l'accumulateur *)
    @@ addi t2 t2 (-48) (* Conversion caractère en nombre *)
    @@ mul  t1 t1 t3
    @@ add  t1 t1 t2 (* t1 <- 10 * t1 + t2 *)

    (* Suite de la lecture *)
    @@ addi t0 t0 1
    @@ b "atoi_loop"

    (* Arrêt du programme en cas d'erreur de lecture *)
    @@ label "atoi_error"
    @@ li   v0 10
    @@ syscall

    (* Renvoi du résultat via [v0] en cas de succès *)
    @@ label "atoi_end"
    @@ move v0 t1
    @@ jr   ra
  in

  (* Construction du texte du programme *)
  let main_code = translate_instruction program.main in
  let text = init @@ main_code @@ close @@ built_ins in

  (* Initialisation de la partie des données statiques *)
  let data = Symb_Tbl.fold
    (fun var _ code -> label var @@ dword [0] @@ code)
    program.globals nop
  in

  (* Programme généré *)
  { text; data }
