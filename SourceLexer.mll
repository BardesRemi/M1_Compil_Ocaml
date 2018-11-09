{

  (* Contexte *)
  open Lexing
  open SourceParser
    
  (* Traitement des chaînes de caractères alphabétiques *)
  let id_or_keyword =
    (* Définition d'une table des mots-clés et des lexèmes associés *)
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [	"main", MAIN;
	"while", WHILE;
	"for", FOR;
	"print", PRINT;
	"if", IF;
	"else", ELSE;
	"elif", ELIF;
	"integer", INTEGER;
	"boolean", BOOLEAN;
	"var", VAR;
	"true", CONST_BOOL(true);
	"false", CONST_BOOL(false);
	"continue", CONTINUE;
	"break", BREAK;
	"new", NEW;
	"struct", STRUCT;
	"immutable", IMMUT;
      ] ;
    fun s ->
      (* On cherche la chaîne [s] dans la table. Si on trouve un mot-clé alors
         on le renvoie. *)
      try  Hashtbl.find h s
      (* Et sinon on considère qu'il s'agit d'un identifiant. *)
      with Not_found -> IDENT(s)
  
}

(* Raccourci : caractères alphabétiques *)
let alpha = ['a'-'z' 'A'-'Z']
(* Raccourci : nombres entiers décimaux *)
let number = ['1'-'9']['0'-'9']* | '0'

(* Expressions régulières définissant les lexèmes *)
rule token = parse
  (* Les espaces sont ignorés *)
  | ' ' | '\t' 
      { token lexbuf }
  | '\n'
      { new_line lexbuf; token lexbuf }
  (* Gestion des commentaires *)
  | "//"
      { short_comment lexbuf }
  | "/*"
      { long_comment lexbuf }
  (* Les chaînes alphabétiques sont traitées par la fonction [id_or_keyword]
     pour être associées à des mots-clés ou des identifiants. *)
  | (alpha|"_")+
      { id_or_keyword (lexeme lexbuf) }
  (* Nombres entiers décimaux *)
  | number as n
      { CONST_INT(int_of_string n) }
  (* Début et fin de bloc *)
  | "{"
      { BEGIN }
  | "}"
      { END }
  | "("
      { LP }
  | ")"
      { RP }
  (* Opérateurs *)
  | "+"
      { PLUS }
  | "-"
      { MINUS }
  | "*"
      { STAR }
  | "/"
      { DIV }
  | "%"
      { MOD }
  | "=="
      { EQUAL }
  | "!="
      { NEQ }
  | "<"
      { LT }
  | ">"
      { GT }
  | "<="
      { LE }
  | ">="
      { GE }
  | "!"
      { NOT }
  | "&&"
      { AND }
  | "||"
      { OR }
  (* Affectation *)
  | ":="
      { SET }
  (* Séparateurs *)
  | ";"
      { SEMI }
  | ","
      { COMMA }
  | "."
      { DOT }
  | "="
      { SEQUAL }
  | "["
      { LB }
  | "]"
      { RB }
  (* Fin de fichier *)
  | eof
      { EOF }
  (* Caractères non reconnus *)
  | _
      { 
	failwith ("Unknown character : (line : " ^ (string_of_int lexbuf.lex_curr_p.pos_lnum) ^
		     ", character :" ^ (string_of_int (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)) ^ ") " ^ (lexeme lexbuf)) }


and short_comment = parse
    |"\n"
	{ new_line lexbuf; token lexbuf }
    |eof
	{ EOF }
    |_
	{ short_comment lexbuf }

and long_comment = parse
    | "*/"
	{ token lexbuf }
    | eof
	{ failwith ("comment not closed before end of file") }
    | "\n"
	{ new_line lexbuf; long_comment lexbuf }
    | _
	{ long_comment lexbuf }
