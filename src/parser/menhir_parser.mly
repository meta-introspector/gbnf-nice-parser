(* tokens *)
%token EOF LPAR RPAR /* COMMA LCUR RCUR LBRA RBRA */
%token <string> ATOM

(* start symbol *)
%start <Ast.sexp> sexp_eof

%%

sexp_eof:
  | e=sexp; EOF { e }
  ;

sexp:
  | a=ATOM { Ast.Atom a }
  | LPAR; es=list(sexp); RPAR { Ast.List es }
  /* | term {} */
;

/* lhs:  */
/*   | ATOM {} */
/* ; */

/* term: */
/*   | LPAR S rhs S RPAR {} */
/*   | LBRA S rhs S RBRA {} */
/*   | LCUR S rhs S RCUR {} */
/*   | terminal {} */
/*   | identifier {} */
/* ; */

/* factor : */
/*   | term; S; "?"; {} */
/*   | term; S; "*"; {} */
/*   | term; S; "+"; {} */
/*   | term; S; "-"; S; term {} */
/*   | term; S; {} */
/* ; */

/* concatenation : */
/*   | list(concatenation_part){} */
/* ; */

/* concatenation_part : */
/*   | S; factor; S {} */
/*   | S; factor; S; COMMA {} */
/* ; */

/* alternation : */
/*   |  list(alternation_parts) {} */
/* ; */

/* alternation_part : */
/*   | S; concatenation; S; "|" {} */
/*   | S; concatenation; S {} */
/* ; */

/* rhs : */
/*   |alternation{} */
/* ; */

/* rule : */
/*   | lhs S "=" S rhs S terminator {}; */

/* rules : */
/*   | S; rule; S {}; */

/* root : */
/*   | list(rules) {}; */

/* terminal : */
/*   | "'" character  "'"  ( character  "'" )  "'" {}; */
/* terminator : */
/*   | (";" | ".") {}; */

%%
