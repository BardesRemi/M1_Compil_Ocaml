%{

  (* Contexte *)
  open Lexing
  open CommonAST
  open SourceLocalisedAST

  let symbls = ref [("arg", TypInt)] (* Liste des symboles à ajouter à la table des symboles *)
  let cpt = ref 0

  let instr i pos = mk_instr i (fst pos) (snd pos)
  let expr e pos = mk_expr e (fst pos) (snd pos)

  (* Création d'une variable temporaire *)
  let create_temp_var typ  =
    let name = "__temp__"^(string_of_int !cpt) in
    cpt := !cpt + 1;
    symbls := (name,typ)::(!symbls);
    name

  (* Ajoute à la talbe des symboles tbl la liste de variables vars de type typ *)
  let add_vars tbl typ vars = List.fold_left (fun acc x -> Symb_Tbl.add x typ acc) tbl vars

  (* Ajoute à la table des symboles tbl la liste de symboles symbls *)
  let add_symbls tbl symbls = List.fold_left (fun acc (name, typ) -> Symb_Tbl.add name typ acc) tbl symbls

  (* Crée une séquence d'instruction à partir de la liste d'instruction l *)
  let rec instruction_list l = 
    match l with
    | [] -> instr Nop (0,0)
    | i::n -> instr (Sequence(i, instruction_list n)) i.i_pos

  (* Affecte la liste d'expressions exprs à la liste d'identifiants ids *)
  let affect_sequence ids exprs =
    let rec affect_sequence_localised ids exprs =
      match ids, exprs with 
      | id::[],e::[] -> Set(Identifier (Id id), e)
      | id1::id2::[], e1::e2::[] -> Sequence((instr (Set(Identifier (Id id1), e1)) e1.e_pos),
					     (instr (Set(Identifier (Id id2), e2)) e1.e_pos))
      | [], e::s -> failwith (Printf.sprintf "Syntax error : more expressions than identifiers at %d, %d" (fst e.e_pos) (snd e.e_pos))
      | id::s, [] -> failwith (Printf.sprintf "Syntax error : more identifiers than expressions")
      | id::s1, e::s2 -> Sequence(instr (Set(Identifier (Id id), e)) e.e_pos,
				  instr (affect_sequence_localised s1 s2) e.e_pos)
      | _, _ -> failwith (Printf.sprintf "Syntax error")
    in
    match ids, exprs with
    | id::s1, e::s2 -> Sequence(instr (Set(Identifier (Id id), e)) e.e_pos,
				instr (affect_sequence_localised s1 s2) e.e_pos)
    | _, _ -> failwith (Printf.sprintf "Syntax error")

  (* Création d'un tableau de dimension n, de type typ dans la 'location' loc, dont la taille successive
     des tableaux est dans size_list *) 
  let rec make_array loc typ size_list =
    let rec array_type nbr =
      match nbr with
      | 0 -> typ
      | n -> TypArray(array_type (nbr-1))
    in
    let temp_type = array_type ((List.length size_list) - 1) in
    match size_list with
    | [] -> failwith (Printf.sprintf "Syntax error: no size for array")
    | s::[] -> Set(loc, expr (NewArray(s, temp_type)) s.e_pos)
    | s::tail -> 
      begin
	let var = create_temp_var TypInt in
	Sequence(
	  (instr (Set(loc, (expr (NewArray(s, temp_type)) s.e_pos))) s.e_pos),
	  (instr (ForLoop(
	    (instr (Set(Identifier(Id var), (expr (Literal(Int(0))) s.e_pos))) s.e_pos),
	    (expr (BinaryOp (Lt, (expr (Location(Identifier (Id var))) s.e_pos), s)) s.e_pos),
	    (instr (Set((Identifier(Id var)), (expr (BinaryOp(Add,
							      (expr (Location(Identifier (Id var))) s.e_pos),
							      (expr (Literal(Int(1))) s.e_pos))) s.e_pos))) s.e_pos),
	    (instr (make_array (ArrayAccess((expr (Location(loc)) s.e_pos), (expr (Location(Identifier (Id var))) s.e_pos))) typ tail) s.e_pos)
	   )) s.e_pos))
      end

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
%token IMMUT

%token MAIN
%token VAR
%token STRUCT
%token INTEGER BOOLEAN VOID

%token IF ELSE ELIF WHILE FOR
%token SEMI COMMA DOT
%token SET
%token BEGIN END
%token EOF
%token NEW
%token RETURN

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
| decls=decls; fun_decl=fun_decl; EOF
  (* Les déclarations de variables donnent une table des symboles, à laquelle
     est ajoutée la variable spéciale [arg] (avec le type entier). *)
  { let symbl_table = add_symbls (fst (fst decls)) (!symbls) in
    let pos = $startpos in
    let i_pos = (pos.pos_lnum, pos.pos_cnum) in 
    { main = instr (Sequence(instruction_list (snd (fst decls)), instr (ProcedureCall(Id ("main"), [expr (Location(Identifier(Id "arg"))) i_pos])) i_pos)) i_pos;
      globals = symbl_table;
      structs = snd decls;
      functions = fun_decl;} }
  
(* Aide : ajout d'une règle pour récupérer grossièrement les erreurs se 
   propageant jusqu'à la racine. *)
| error { let pos = $startpos in
          let message =
            Printf.sprintf "Syntax error at %d, %d" pos.pos_lnum pos.pos_cnum
          in
          failwith message }
;

(* Séquence de déclarations *)
decls:
(* Si pas de déclaration, on renvoie la table vide. *)
| (* empty *)  { ((Symb_Tbl.empty, []), Symb_Tbl.empty) }
| VAR; t=typ; vars_list=multiple_vars; decls=decls { ((add_vars (fst (fst decls)) t vars_list, (snd (fst decls))), (snd decls)) }
| VAR; t=typ; id=IDENT; SEQUAL; e=localised_expression; SEMI; decls=decls { ((Symb_Tbl.add id t (fst (fst decls)), (instr (Set(Identifier (Id id), e)) e.e_pos)::(snd (fst decls))), (snd decls)) }
| STRUCT; id=IDENT; BEGIN; fields=field_decl; END; decls=decls { ((fst decls), Symb_Tbl.add id { fields = fields } (snd decls))  }
;

fun_decl:
| (* empty *) { Symb_Tbl.empty }
| t=typ; id=IDENT; LP; fp = formal_params; RP; BEGIN; localsVar=decls; i=localised_complexe_instruction; END; fd=fun_decl
   { Symb_Tbl.add id ({locals=(fst (fst localsVar)) ;signature={ return=t; formals=fp } ; code=i}) fd }
| id=IDENT; LP; fp = formal_params; RP; BEGIN; localsVar=decls; i=localised_complexe_instruction; END; fd=fun_decl
   { Symb_Tbl.add id ({locals=(fst (fst localsVar)) ;signature={ return=TypVoid; formals=fp } ; code=i}) fd }
| MAIN; LP; fp = formal_params; RP; BEGIN; localsVar=decls; i=localised_complexe_instruction; END; fd=fun_decl
   { Symb_Tbl.add "main" ({locals=(fst (fst localsVar)) ;signature={ return=TypInt; formals=fp } ; code=i}) fd }
| MAIN; BEGIN; localsVar=decls; i=localised_complexe_instruction; END; fd=fun_decl
   { Symb_Tbl.add "main" ({locals=(fst (fst localsVar)) ;signature={ return=TypInt; formals=[("arg",TypInt)] } ; code=i}) fd }
;
    
(* Déclaration de plusieurs variables sur la meme ligne *)
multiple_vars:
| id=IDENT; SEMI { [id] }
| id=IDENT; COMMA; vars=multiple_vars { id::vars }
;

(* Déclaration d'une structure *)
field_decl:
| IMMUT; t=typ; id=IDENT; SEMI; fields=field_decl { (id, t, true)::fields }
| t=typ; id=IDENT; SEMI; fields=field_decl { (id, t, false)::fields }
| IMMUT; t=typ; id=IDENT; SEMI { [(id, t, true)] }
| t=typ; id=IDENT; SEMI { [(id, t, false)] }
;

(* Déclarations des paramètres formels d'une fonction *)
formal_params:
| (* empty *) { [] } 
| t=typ; id=IDENT { [(id,t)] }
| t=typ; id=IDENT; COMMA; fp = formal_params { (id,t)::fp }

(* Un bloc est une instruction ou séquence d'instructions entre accolades. *)
block:
| BEGIN; i=localised_complexe_instruction; END { i }
;

typ:
| INTEGER { TypInt }
| BOOLEAN { TypBool }
| t=typ; LB; RB { TypArray(t) }
| id=IDENT { TypStruct(id) }
| VOID { TypVoid }

location:
| id=IDENT { Identifier (Id id) }
| e1=location; LB; e2=localised_expression ; RB { ArrayAccess(expr (Location(e1)) e2.e_pos, e2) }
| struct_id=location_structural_rec; DOT; field_id=IDENT { FieldAccess(struct_id, field_id) }
;

location_structural_rec:
| id=IDENT; { let l = $startpos.pos_lnum in
              let c = $startpos.pos_cnum - $startpos.pos_bol in
              mk_expr (Location(Identifier(Id id))) l c }
| id=location_structural_rec; DOT; f=IDENT { let l = $startpos.pos_lnum in
					     let c = $startpos.pos_cnum - $startpos.pos_bol in
					     mk_expr (Location(FieldAccess(id, f))) l c }
;
   
(* Instruction localisée : on mémorise les numéros de ligne et de colonne du
   début de l'instruction.
   Voir dans la doc la définition de [Lexing.position] pour la signification
   de [pos_lnum], [pos_cnum] et [pos_bol]. *)
localised_simple_instruction:
| i=simple_instruction { let l = $startpos.pos_lnum in
                  let c = $startpos.pos_cnum - $startpos.pos_bol in
                  mk_instr i l c }
;

localised_complexe_instruction:
| i=complexe_instruction { let l = $startpos.pos_lnum in
                  let c = $startpos.pos_cnum - $startpos.pos_bol in
                  mk_instr i l c }
;

(* Instructions *)
simple_instruction:
(* Si pas d'instruction, on renvoie l'instruction neutre. *)
| (* empty *)  { Nop }
| BREAK { Break }
| CONTINUE { Continue }
| l=location; SET; e=localised_expression { Set(l, e) }
| l=location; SET; NEW; t=typ; a=array_decl { make_array l t a }
| RETURN; LP; e=localised_expression; RP { Return(e) }
| id=IDENT; LP; args=arguments; RP { ProcedureCall(Id (id), args) } 
;

complexe_instruction:
| i=simple_instruction { i }
| IF; LP; e=localised_expression; RP; i=block { Conditional(e, i, mk_instr Nop 0 0) }
| IF; LP; e=localised_expression; RP; i=block; ELIF; cc=cascade_conditional { Conditional(e, i, cc) }
| IF; LP; e=localised_expression; RP; i1=block; ELSE; i2=block { Conditional(e, i1, i2) }
| WHILE; LP; e=localised_expression; RP; i=block { Loop(e, i) }
| FOR; LP; i1=localised_simple_instruction; COMMA; e=localised_expression; COMMA; i2=localised_simple_instruction; RP; i3=block { ForLoop(i1, e, i2, i3) }
| i1=localised_complexe_instruction; SEMI; i2=localised_complexe_instruction { Sequence(i1, i2) }
| ids=ident_list; SET; e_list=expr_list { affect_sequence ids e_list }
;

array_decl:
|LB; e=localised_expression; RB; { [e] }
|LB; e=localised_expression; RB; a=array_decl { e::a }
   

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
| id=location { Location (id) }
| LP; e=localised_expression; RP { e.expr }
| MINUS; e=localised_expression %prec UMINUS { UnaryOp(Minus, e) }
| NOT; e=localised_expression { UnaryOp(Not, e) }
| NEW; id_struct=IDENT { NewRecord(id_struct) }
| id=IDENT; LP; args=arguments; RP { FunCall(Id (id), args) } 
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

(* Renvoi la liste des args *)
arguments:
| args=separated_list(COMMA, localised_expression) { args }
