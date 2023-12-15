(* tokens *)
%token EOF LPAR RPAR
%token <string> ATOM

(* start symbol *)
%start <Ast.sexp> sexp_eof

%%

sexp_eof:
  | e=sexp; EOF { e }
  ;

    (* terminal ::= "'" character  "'"  ( character  "'" )  "'" *)
(* terminator ::= (";" | ".") *)

sexp:
  | a=ATOM { Ast.Atom a }
  | LPAR; es=list(sexp); RPAR { Ast.List es }
  | term
  ;

term : LPAR S rhs S RPAR
  | LBRA S rhs S RBRA
  | LCUR S rhs S RCUR
  | terminal
  | identifier
;
(* factor : term S "?" | term S "*" | term S "+" | term S "-" S term | term S *)
(* concatenation : ( S factor S "," ? ) + *)
(* alternation : ( S concatenation S "|" ? ) + *)
(* rhs : alternation *)
(* lhs : identifier *)
(* rule : lhs S "=" S rhs S terminator *)
(* root : ( S rule S ) * *)

%%
