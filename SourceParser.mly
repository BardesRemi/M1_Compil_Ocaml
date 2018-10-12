%{

  (* Contexte *)
  open Lexing
  open CommonAST
  open SourceLocalisedAST

  let add_vars tbl typ vars = 
	let rec add_vars_rec table var_list =
  		match var_list with
		| [] -> table
		| x::s -> add_vars_rec (Symb_Tbl.add x typ table) s	
	in
	add_vars_rec tbl vars
 
  (*let add_vars tbl typ vars = List.fold_left (fun acc x -> Symb_Tbl.add x typ acc) tbl vars *)

  let rec instruction_list l = 
    match l with
    | [] -> mk_instr Nop 0 0
    | x::n -> mk_instr (Sequence(x, instruction_list n)) (fst x.i_pos) (snd x.i_pos)
 
  let affect_sequence ids exprs =
    let rec affect_sequence_localised ids exprs =
      match ids, exprs with 
      | id::[],e::[] -> mk_instr (Set(Identifier (Id id), e)) (fst e.e_pos) (snd e.e_pos)
      | id1::id2::[], e1::e2::[] -> mk_instr (Sequence((mk_instr (Set(Identifier (Id id1), e1)) (fst e1.e_pos) (snd e1.e_pos)),
						       (mk_instr (Set(Identifier (Id id2), e2)) (fst e1.e_pos) (snd e1.e_pos)))) (fst e1.e_pos) (snd e1.e_pos)
      | [], e::s -> failwith (Printf.sprintf "Syntax error : more expressions than identifiers at %d, %d" (fst e.e_pos) (snd e.e_pos))
      | id::s, [] -> failwith (Printf.sprintf "Syntax error : more identifiers than expressions")
      | id::s1, e::s2 -> mk_instr (Sequence(mk_instr (Set(Identifier (Id id), e)) (fst e.e_pos) (snd e.e_pos),
					    affect_sequence_localised s1 s2)) (fst e.e_pos) (snd e.e_pos)
      | _, _ -> failwith (Printf.sprintf "Syntax error")
    in
    match ids, exprs with
    | id::s1, e::s2 -> Sequence(mk_instr (Set(Identifier (Id id), e)) (fst e.e_pos) (snd e.e_pos),
				affect_sequence_localised s1 s2)
      | _, _ -> failwith (Printf.sprintf "Syntax error")
%}

(* Définition des lexèmes *)
%token <int> CONST_INT
%token <bool> CONST_BOOL
%token <string> IDENT
%token PLUS MINUS STAR DIV MOD
%token EQUAL NEQ LE LT GE GT
%token SEQUAL
%token AND OR NOT
%token LP RP LB RB
%token BREAK CONTINUE

%token VAR
%token INTEGER BOOLEAN

%token MAIN
%token IF ELSE ELIF WHILE FOR
%token SEMI COMMA
%token SET PRINT
%token BEGIN END
%token EOF
%token NEW

%left OR
%left AND
%left EQUAL NEQ
%left LE LT GE GT
%left PLUS MINUS
%left STAR DIV MOD
%right NOT UMINUS
%left SEMI

(* Définition du symbole initial *)
%start prog
%type <SourceLocalisedAST.program> prog

%%

(* Symbole non-terminal principal [prog] *)
prog:
(* Règles : un programme est formé d'une séquence de déclarations de variables
   suivie du bloc de code principal. *)
| vars=var_decls; main=main; EOF
  (* Les déclarations de variables donnent une table des symboles, à laquelle
     est ajoutée la variable spéciale [arg] (avec le type entier). *)
  { { main = mk_instr (Sequence(instruction_list (snd vars), main)) (fst main.i_pos) (snd main.i_pos);
      globals = Symb_Tbl.add "arg" TypInt (fst vars); } }
  
(* Aide : ajout d'une règle pour récupérer grossièrement les erreurs se 
   propageant jusqu'à la racine. *)
| error { let pos = $startpos in
          let message =
            Printf.sprintf "Syntax error at %d, %d" pos.pos_lnum pos.pos_cnum
          in
          failwith message }
;

(* Séquence de déclaration de variables *)
var_decls:
(* Si pas de déclaration, on renvoie la table vide. *)
| (* empty *)  { (Symb_Tbl.empty, []) }
| VAR; t=typ; vars_list=multiple_vars; vars=var_decls { (add_vars (fst vars) t vars_list, (snd vars)) }
| VAR; t=typ; id=IDENT; SEQUAL; e=localised_expression; SEMI; vars=var_decls { (Symb_Tbl.add id t (fst vars), (mk_instr (Set(Identifier (Id id), e)) (fst e.e_pos) (snd e.e_pos))::(snd vars)) }
;

multiple_vars:
| id=IDENT; SEMI { [id] }
| id=IDENT; COMMA; vars=multiple_vars { id::vars }
;

(* Bloc de code principal, formé du mot-clé [main] suivi par le bloc
   proprement dit. *)
main:
| MAIN; i=block { i }
;

(* Un bloc est une instruction ou séquence d'instructions entre accolades. *)
block:
| BEGIN; i=localised_instruction; END { i }
;

typ:
| INTEGER { TypInt }
| BOOLEAN { TypBool }
| t=typ; LB; RB { TypArray(t) }

(* Instruction localisée : on mémorise les numéros de ligne et de colonne du
   début de l'instruction.
   Voir dans la doc la définition de [Lexing.position] pour la signification
   de [pos_lnum], [pos_cnum] et [pos_bol]. *)
localised_instruction:
| i=instruction { let l = $startpos.pos_lnum in
                  let c = $startpos.pos_cnum - $startpos.pos_bol in
                  mk_instr i l c }
;

(* Instructions *)
instruction: 
(* Si pas d'instruction, on renvoie l'instruction neutre. *)
| (* empty *)  { Nop }
| BREAK { Break }
| CONTINUE { Continue }
| PRINT; LP; e=localised_expression; RP { Print(e) }
| id=IDENT; SET; e=localised_expression { Set(Identifier (Id id), e) }
| ids=ident_list; SET; e_list=expr_list { affect_sequence ids e_list }
| IF; LP; e=localised_expression; RP; i=block { Conditional(e, i, mk_instr Nop 0 0) }
| IF; LP; e=localised_expression; RP; i=block; ELIF; cc=cascade_conditional { Conditional(e, i, cc) }
| IF; LP; e=localised_expression; RP; i1=block; ELSE; i2=block { Conditional(e, i1, i2) }
| WHILE; LP; e=localised_expression; RP; i=block { Loop(e, i) }
| FOR; LP; i1=localised_instruction; SEMI; e=localised_expression; SEMI; i2=localised_instruction; RP; i3=block { ForLoop(i1, e, i2, i3) }
| i1=localised_instruction; SEMI; i2=localised_instruction { Sequence(i1, i2) }
;

ident_list:
| id=IDENT; COMMA; id_list=ids { id::id_list }

ids:
| id=IDENT { [id] }
| id=IDENT; COMMA; id_list=ids { id::id_list }

expr_list:
| e=localised_expression; COMMA; e_list=exprs { e::e_list }

exprs:
| e=localised_expression { [e] }
| e=localised_expression; COMMA; e_list=exprs { e::e_list }

cascade_conditional:
| LP; e=localised_expression; RP; i=block; ELIF; cc=cascade_conditional 
	{ let instr = Conditional(e, i, cc) in
	  let (a, b) = e.e_pos in	
	  mk_instr instr a b }
| LP; e=localised_expression; RP; i1=block; ELSE; i2=block 
	{ let instr = Conditional(e, i1, i2) in
	  let (a, b) = e.e_pos in	
	  mk_instr instr a b }

localised_expression:
| e=expression { let l = $startpos.pos_lnum in
                 let c = $startpos.pos_cnum - $startpos.pos_bol in
                 mk_expr e l c }
;

(* Expressions *)
expression:
(* Si pas d'exression, on renvoie une erreur *)
| i=CONST_INT { Literal (Int i) }
| b=CONST_BOOL { Literal (Bool b) } 
| id=IDENT { Location (Identifier (Id id)) }
| e1=localised_expression; LB; e2=localised_expression; RB { Location (ArrayAccess (e1, e2)) }
| LP; e=localised_expression; RP { e.expr }
| MINUS; e=localised_expression %prec UMINUS { UnaryOp(Minus, e) }
| NOT; e=localised_expression { UnaryOp(Not, e) }
| NEW; t=typ; LB; e=localised_expression; RB { NewArray(e, t) }
| e1=localised_expression; PLUS; e2=localised_expression { BinaryOp(Add, e1, e2) }
| e1=localised_expression; MINUS; e2=localised_expression { BinaryOp(Sub, e1, e2) }
| e1=localised_expression; STAR; e2=localised_expression { BinaryOp(Mult, e1, e2) }
| e1=localised_expression; DIV; e2=localised_expression { BinaryOp(Div, e1, e2) }
| e1=localised_expression; MOD; e2=localised_expression { BinaryOp(Mod, e1, e2) }
| e1=localised_expression; EQUAL; e2=localised_expression { BinaryOp(Eq, e1, e2) }
| e1=localised_expression; NEQ; e2=localised_expression { BinaryOp(Neq, e1, e2) }
| e1=localised_expression; LT; e2=localised_expression { BinaryOp(Lt, e1, e2) }
| e1=localised_expression; LE; e2=localised_expression { BinaryOp(Le, e1, e2) }
| e1=localised_expression; GT; e2=localised_expression { BinaryOp(Gt, e1, e2) }
| e1=localised_expression; GE; e2=localised_expression { BinaryOp(Ge, e1, e2) }
| e1=localised_expression; AND; e2=localised_expression { BinaryOp(And, e1, e2) }
| e1=localised_expression; OR; e2=localised_expression { BinaryOp(Or, e1, e2) }
;
