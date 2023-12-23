/******************************************************************************/
/*                                                                            */
/*                                   Menhir                                   */
/*                                                                            */
/*                       François Pottier, Inria Paris                        */
/*              Yann Régis-Gianas, PPS, Université Paris Diderot              */
/*                                                                            */
/*  Copyright Inria. All rights reserved. This file is distributed under the  */
/*  terms of the GNU General Public License version 2, as described in the    */
/*  file LICENSE.                                                             */
/*                                                                            */
/******************************************************************************/

/* This is the fancy version of the parser, to be processed by menhir.
   It is kept in sync with [Parser], but exercises menhir's features. */

/* As of 2014/12/02, the $previouserror keyword and the --error-recovery
   mode no longer exist. Thus, we replace all calls to [Error.signal]
   with calls to [Error.error], and report just one error. */

/* ------------------------------------------------------------------------- */
/* Imports. */

%{

open Stretch
open Syntax
open Positions

let rec find s n i =
  assert (i < n);
  if s.[i] = '(' then i
  else begin
    assert (s.[i] = ' ');
    find s n (i+1)
  end

let unparenthesize (s : string) : string =
  let n = String.length s in
  (* The string [s] must end with a closing parenthesis. *)
  assert (n >= 2 && s.[n-1] = ')');
  (* The string [s] must begin with a certain amount of spaces
     followed with an opening parenthesis. Find its offset [i]. *)
  let i = find s n 0 in
  (* Create a copy without the parentheses. *)
  let b = Bytes.of_string s in
  Bytes.set b i ' ';
  Bytes.set b (n-1) ' ';
  Bytes.to_string b

let unparenthesize (s : Stretch.t) : Stretch.t =
  { s with stretch_content = unparenthesize s.stretch_content }

let unparenthesize (o : Stretch.t option) : Stretch.t option =
  Option.map unparenthesize o

%}

/* ------------------------------------------------------------------------- */
/* Tokens. */
%token <int> Tchar
%token DASH "-"
%token CARET "^"
%token
  TOKEN            "%token"
  TYPE             "%type"
  LEFT             "%left"
  RIGHT            "%right"
  NONASSOC         "%nonassoc"
  START            "%start"
  PREC             "%prec"
  PUBLIC           "%public"
  COLON            ":"
  BAR              "|"
  EOF              ""
  EQUAL            "="
  INLINE           "%inline"
  LPAREN           "("
  RPAREN           ")"
  LBRACE "["
  RBRACE "]"
  COMMA            ","
  QUESTION         "?"
  STAR             "*"
  PLUS             "+"
  PARAMETER        "%parameter"
  ON_ERROR_REDUCE  "%on_error_reduce"
SEMI             ";"
NEWLINE
WHITESPACE

%token <string Positions.located>
  LID              "lident"
  QID              "\"alias\""

%token <Stretch.ocamltype>
  OCAMLTYPE        "<unit>"



/* For the new rule syntax: */
%token
  LET              "let"
  TILDE            "~"
  UNDERSCORE       "_"
  COLONEQUAL       ":="
  COLONCOLONEQUAL  "::="
  EQUALEQUAL       "=="

(* %type <ParserAux.early_producer> producer *)
(* %type <ParserAux.early_production> production *)
%start <Syntax.partial_grammar> grammar


%nonassoc BAR

/* ------------------------------------------------------------------------- */
/* On-error-reduce declarations. */

/* These declarations reduce the number of states where an error can occur,
   thus reduce the number of syntax error messages that we have to write in
   parserMessages.messages. */

%on_error_reduce old_rule




%%

/* ------------------------------------------------------------------------- */
/* A grammar consists of  rules 
taken from https://github.com/dmbaturin/bnfgen
*/
rules:
  | {}
  | old_rule {
	(print_endline (Batteries.dump ("DEBUG:rs",$1)))
      }
  | old_rule NEWLINE+ old_rule {
			  (print_endline (Batteries.dump ("DEBUG:rs",$1,$3)))
			}

grammar:
  rs =  rules
    {
      (print_endline (Batteries.dump ("DEBUG:rs",rs)));
      {
        pg_filename          = ""; (* filled in by the caller *)
        pg_rules             = [];
      }
    }

%inline rule_specific_token:
| PUBLIC
| INLINE
| COLON
| LET
| EOF
    { () }


%inline clist(X):
  xs = separated_nonempty_list(COMMA?, X)
    { xs }


symbol:
id = LID
    {
      (print_endline (Batteries.dump ("DEBUG:LID", id)));
      id }
  | id = QID
    {
      (print_endline (Batteries.dump ("DEBUG:QID", id)));
      id }

old_rule:
symbol = symbol
/* the symbol that is being defined */
COLONCOLONEQUAL
branches = rhs(* separated_nonempty_list(BAR, symbol+) *)
    {
      (print_endline (Batteries.dump ("DEBUG:branches1", branches)));
      {
        pr_nt          = Positions.value symbol;
        pr_positions   = [ Positions.position symbol ];
        pr_branches    =  [] (*Fixme should be brancheS*)
      }
    }







modifier:
  QUESTION
    { "option" }
| PLUS
    { "nonempty_list" }
| STAR
    { "list" }

postlude:
  EOF
    { None }



reversed_preceded_or_separated_nonempty_llist(delimiter, X):
| ioption(delimiter) x = X
    { [x] }
| xs = reversed_preceded_or_separated_nonempty_llist(delimiter, X)
  delimiter
  x = X
    { x :: xs }

%inline preceded_or_separated_nonempty_llist(delimiter, X):
  xs = rev(reversed_preceded_or_separated_nonempty_llist(delimiter, X))
    { xs }

preceded_or_separated_llist(delimiter, X):
| (* empty *)
    { [] }
| xs = preceded_or_separated_nonempty_llist(delimiter, X)
    { xs }


located(X):
  x = X
    { with_loc $loc x }

term:
  | "(" rhs  ")" {}
  | "[" rhs  "]" {}
  /* | "{" rhs  "}" {} */
  | symbol {}

factor:
  | term "?" {}
  | term "*" {}
  | term "+" {}
  | term "-" term {}
  | term  {}

concatenation:
  | factor + {}

rhs:
  | concatenation {}
  | concatenation BAR concatenation{}
  |  {}

/* expression: */
/*   | prods = expression BAR expression */
/*     {  */
/*       (print_endline (Batteries.dump ("DEBUG:branches2",prods))) */
/*     } */
/*   | prods = expression expression */
/*     {  */
/*       (print_endline (Batteries.dump ("DEBUG:branches2",prods))) */
/*     } */

/*   /\* | expression expression  { (print_endline (Batteries.dump ("DEBUG:rs",$1))) } *\/ */
/*   | LPAREN expression RPAREN { (print_endline (Batteries.dump ("DEBUG:rs",$2))) } */
/*   | LBRACE list(char_class) RBRACE { (print_endline (Batteries.dump ("DEBUG:rs",$2))) } */
/*   | e = expression m = modifier    {	       (print_endline (Batteries.dump ("DEBUG:branches3", e,m )))     } */
/*   | branches = symbol   {      (print_endline (Batteries.dump ("DEBUG:branches4", branches)))    } */
/* ; */

(* ocaml/lex/parser.mly *)
char_class:
    CARET char_class1
    /* { Cset.complement $2 } */
{   (print_endline (Batteries.dump ("DEBUG:rs",$2))) }
  | char_class1
    /* { $1 } */
    {   (print_endline (Batteries.dump ("DEBUG:rs",$1))) }
;
char_class1:
    Tchar DASH Tchar
    /* { Cset.interval $1 $3 } */
    {   (print_endline (Batteries.dump ("DEBUG:rs",$1,$2))) }
  | Tchar
    /* Cset.singleton $1 */
    {   (print_endline (Batteries.dump ("DEBUG:rs",$1))) }
  /* | char_class1 char_class1 %prec CONCAT */
  /*       { Cset.union $1 $2 } */
;

/* symbol_expression: */
/* /\* | symbol = symbol es = plist(expression)  *\/ */
/* /\*     {   (print_endline (Batteries.dump ("DEBUG:rs",symbol, es))) } *\/ */
/* | e = located(symbol_expression) m = located(modifier)  */
/*     { */
/*        (print_endline (Batteries.dump ("DEBUG:rs",e,m))) */
/*     } */



%%
