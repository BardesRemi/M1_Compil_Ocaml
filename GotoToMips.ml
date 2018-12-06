open CommonAST
open GotoAST
open Mips

(* Type de gestion des variables locales et formelles d'une fonction *)
type function_vars = {
  localVars: int Symb_Tbl.t;
  formalVars: int Symb_Tbl.t
}

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
let translate_instruction_bis (i: GotoAST.instruction) context = 
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
       begin
	 try
	   let local_pos = Symb_Tbl.find name context.localVars in
	   (* La variable est une locale *)
	   lw t0 local_pos fp
	 with
	 | Not_found ->
	    begin
	      try
		let formal_pos = Symb_Tbl.find name context.formalVars in
		(* La variable est un paramètre formel de la fonction en cours d'exécution *)
		lw t0 formal_pos fp
	      with
	      | Not_found ->
		 (* La variable est une globale *)
		 la t0 name
		 @@ lw t0 0 t0
	    end
       end
    | GotoAST.Location (BlockAccess(e1, e2)) ->
       translate_expression e1
       @@ push t0
       @@ translate_expression e2
       @@ pop t1
       (* On test si on est dans les bornes du tableaux *)
       @@ subi t1 t1 4
       @@ lw t2 0(t1) (* t2 <- la taille du tableaux *)
       @@ ble t2 t0 "atoi_error"
       @@ bltz t0 "atoi_error"
       @@ li t2 4
       @@ addi t1 t1 4
       @@ mul t0 t0 t2 (* $t0 <- $t0*4 car taille mémoire d'une case = 4 *)
       @@ add t1 t1 t0 (* $t1 <- $t1*$t0 car t1 = adresse 1ère case du tableau | t0 = décalage nécessaire pour l'indice du tableau *)
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
       @@ addi t1 t0 1 (* $t1 <- taille du tableau + en tete *)
       @@ move a0 t1   
       @@ li t1 4      
       @@ mul a0 a0 t1 (* on multiplie par 4 cette taille #4= taille mémoire d'une case *)
       @@ li v0 9
       @@ syscall      (* on alloue la mémoire correspondance *)
       @@ sw t0 0(v0)  (* on met dans l'en-tete t0 qui est la taille du tableau *)
       @@ addi t0 v0 4 (* $t0 <- adresse du premier champ *)
    | GotoAST.FunCall(Id name, param_list) ->
       (* 1/ Protocole d'appel : appelant avant l'appel *)
       (List.fold_left (fun acc e -> (translate_expression e) @@ push t0 @@ acc) nop param_list)
       (* 2/ Appel avec [jal]. *)
       @@ jal name
       (* 3/ Protocole d'appel : appelant après l'appel. on à déjà t0 <- res *)
       @@ addi sp sp (4*(List.length param_list))

  (**
     Fonction de traduction des locations.
     [translate_location : GotoAST.location -> Mips.text]
  *)
  and translate_location = function
    | GotoAST.Identifier(Id name) ->
       begin
	 try
	   let local_pos = Symb_Tbl.find name context.localVars in
	   (* La variable est un paramètre formel de la fonction en cours d'exécution *)
	   addi t0 fp local_pos
	 with
	 | Not_found ->
	    (* La variable est une globale *)
	    la t0 name
       end
    | GotoAST.BlockAccess(e1, e2) ->
       translate_expression e1
       @@ push t0
       @@ translate_expression e2
       @@ pop t1
       (* On test si on est dans les bornes du tableaux *)
       @@ subi t1 t1 4
       @@ lw t2 0(t1) (* t2 <- la taille du tableaux *)
       @@ ble t2 t0 "atoi_error"
       @@ bltz t0 "atoi_error"
       @@ addi t1 t1 4
       @@ li t2 4
       @@ mul t0 t0 t2 
       @@ add t0 t1 t0	 
  in	 
  (**
     Fonction de traduction des instructions.
     [translate_instruction : GotoAST.instruction -> Mips.text]
  *)
  let rec translate_instruction (i: GotoAST.instruction) = match i with
    | GotoAST.Sequence (i1, i2) ->
       translate_instruction i1
       @@ translate_instruction i2
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
    | GotoAST.Return(e) ->
       translate_expression e (* renvois le résultat de e dans t0 *)
       @@ addi sp sp (4*(Symb_Tbl.cardinal context.localVars))
       @@ pop ra
       @@ pop fp
       @@ jr ra (* retourne au code qui suis l'appel de fonction *)
    | GotoAST.ProcedureCall(Id name, e_list) ->
       (* 1/ Protocole d'appel : appelant avant l'appel *)
       (List.fold_left (fun acc e -> (translate_expression e) @@ push t0 @@ acc) nop e_list)
       (* 2/ Appel avec [jal]. *)
       @@ jal name
       (* 3/ Protocole d'appel : appelant après l'appel. on à déjà t0 <- res *)
       @@ addi sp sp (4*(List.length e_list))
  in
  translate_instruction i

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
    @@ li v0 10
    @@ syscall

    (* Renvoi du résultat via [v0] en cas de succès *)
    @@ label "atoi_end"
    @@ move v0 t1
    @@ jr   ra

    (* Fonction prédéfinie *)
    @@ comment "print_int"
    @@ label "print_int_int"
    @@ push fp
    @@ push ra
    @@ addi fp sp 8
    @@ lw t0 4(fp)
    @@ move a0 t0
    @@ li v0 1
    @@ syscall
    @@ pop ra
    @@ pop fp
    @@ jr ra  
      
    @@ comment "power"
    @@ label "power_int_int"
    @@ push fp
    @@ push ra
    @@ addi fp sp 8
    @@ lw s0 8(fp)
    @@ lw s1 4(fp)
    @@ li t0 1
    @@ b "power_loop_guard"
    @@ label "power_loop_code"
    @@ mul t0 t0 s1
    @@ subi s0 s0 1
    @@ label "power_loop_guard"
    @@ bgtz s0 "power_loop_code"
    @@ pop ra
    @@ pop fp
    @@ jr ra

    @@ comment "print"
    @@ label "print_int"
    @@ push fp
    @@ push ra
    @@ addi fp sp 8
    @@ lw t0 4(fp)
    @@ move a0 t0
    @@ li v0 11
    @@ syscall
    @@ pop ra
    @@ pop fp
    @@ jr ra  
  in

  (* Construction du texte du programme *)
  let main_code = translate_instruction_bis program.main { localVars=(Symb_Tbl.empty); formalVars=(Symb_Tbl.empty) } in

  let mips_function k fs acc =
    let context = {
      localVars=snd (Symb_Tbl.fold (fun k t acc -> ((fst acc)-4, Symb_Tbl.add k (fst acc) (snd acc))) fs.locals (-8,(Symb_Tbl.empty)));
      formalVars=snd (List.fold_left (fun acc x -> ((fst acc)+4, Symb_Tbl.add (fst x) (fst acc) (snd acc))) (4, (Symb_Tbl.empty)) fs.signature.formals) } in
    acc
    @@ comment k
    @@ label k
    (* On met fp et ra sur la pile *)
    @@ push fp
    @@ push ra
    (* On place le fp actuel *)
    @@ addi fp sp 8
    @@ subi sp sp (4*(Symb_Tbl.cardinal context.localVars))
    @@ translate_instruction_bis fs.code context
    @@ addi sp sp (4*(Symb_Tbl.cardinal context.localVars))
    @@ pop ra
    @@ pop fp
    @@ jr ra
  in
  (* Initialisation des fonctions déclarées *)
  let functions = Symb_Tbl.fold (fun k f acc -> mips_function k f acc) program.functions nop in
  
  let text = init @@ main_code @@ close @@ built_ins @@ functions in

  (* Initialisation de la partie des données statiques *)
  let data = Symb_Tbl.fold
    (fun var _ code -> label var @@ dword [0] @@ code)
    program.globals nop
  in
    
  (* Programme généré *)
  { text; data }
