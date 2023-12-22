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
  UID              "UIdent"
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


%nonassoc no_optional_bar
%nonassoc BAR

/* ------------------------------------------------------------------------- */
/* On-error-reduce declarations. */

/* These declarations reduce the number of states where an error can occur,
   thus reduce the number of syntax error messages that we have to write in
   parserMessages.messages. */

%on_error_reduce old_rule

%on_error_reduce separated_nonempty_list(COMMA,symbol)




%%

/* ------------------------------------------------------------------------- */
/* A grammar consists of  rules */

grammar:
  rs =  separated_nonempty_list(NEWLINE+, old_rule)
    {
      (* (print_endline (Batteries.dump ("DEBUG:rs",rs))); *)
      { 
        pg_filename          = ""; (* filled in by the caller *)           
        pg_rules             = rs;
      (*   pg_postlude = None; *)
      (*   pg_declarations = []; *)
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
| id = UID
| id = QID
    {
      (print_endline (Batteries.dump ("DEBUG:ID", id)));
      id }

(* %inline terminal_alias_attrs: *)
(*   id = UID alias = QID? attrs = ATTRIBUTE* *)
(*     { (print_endline (Batteries.dump ("DEBUG:attributes", id))); *)
(*       let alias = Option.map Positions.value alias in *)
(*       Positions.map (fun uid -> uid, alias, attrs) id } *)

(* %inline nonterminal: *)
(*   id = LID *)
(*     { id } *)

/* ------------------------------------------------------------------------- */
/* A rule is expressed either in the traditional (yacc-style) syntax or in
   the new syntax. */

(* %inline rule: *)
(*   old_rule *)
(*     { *)
(*       (print_endline (Batteries.dump ($1))); *)
(*       $1 } *)
(* | new_rule *)
(*     /* The new syntax is converted on the fly to the old syntax. */ *)
(*     { (print_endline (Batteries.dump ($1))); *)
(*       NewRuleSyntax.rule $1 } *)

old_rule:
  symbol = symbol          /* the symbol that is being defined */
COLONCOLONEQUAL
  branches = separated_nonempty_list(BAR, symbol+)
	       {
		 (print_endline (Batteries.dump ("DEBUG:branches", branches)));
                 {
                   pr_nt          = Positions.value symbol;
                   pr_positions   = [ Positions.position symbol ];
                   pr_branches    =  [] (*Fixme should be brancheS*)
                 }
    }

(* branches: *)
(*   prods = production_group *)
(*     { *)
(*       (print_endline (Batteries.dump ("DEBUG:branches",prods))); *)
(*       [](\* prods *\) *)
(*     } *)


optional_bar:
  /* epsilon */ %prec no_optional_bar
| BAR
    { () }


production:
  producers = producer* 
    {
      (* oprec = ioption(precedence) *)
      (print_endline (Batteries.dump ("DEBUG:production", producers)));
      (* ParserAux.early_producers *) producers,        
      (* (\* oprec, *\) *) (* string located option  *)      None,
      (* on_error_reduce_level *)      ParserAux.new_production_level(),
      (* Positions.t*)      Positions.import $loc 
    }


producer:
| id = LID
    {
      (print_endline (Batteries.dump ("DEBUG:producer", id)));
      
      (* position (with_loc $loc ()), id, p } *)
    }

(* %inline generic_actual(A, B): *)
(* (\* 1- *\) *)
(*   symbol = symbol actuals = plist(A) *)
(*     { Parameters.app symbol actuals } *)
(* (\* 2- *\) *)
(* | p = B m = located(modifier) *)
(*     { ParameterApp (m, [ p ]) } *)


(* actual: *)
(*   p = generic_actual(lax_actual, actual) *)
(*     { p } *)

(* lax_actual: *)
(*   p = generic_actual(lax_actual, /* cannot be lax_ */ actual) *)
(*     { p } *)
(* (\* 3- *\) *)
(* | /* leading bar disallowed */ *)
(*   branches = located(branches) *)
(*     { ParameterAnonymous branches } *)
(*     (\* 2016/05/18: we used to eliminate anonymous rules on the fly during *)
(*        parsing. However, when an anonymous rule appears in a parameterized *)
(*        definition, the fresh nonterminal symbol that is created should be *)
(*        parameterized. This was not done, and is not easy to do on the fly, *)
(*        as it requires inherited attributes (or a way of simulating them). *)
(*        We now use explicit abstract syntax for anonymous rules. *\) *)

(* /* ------------------------------------------------------------------------- */ *)
(* /* The "?", "+", and "*" modifiers are short-hands for applications of *)
(*    certain parameterized nonterminals, defined in the standard library. */ *)

modifier:
  QUESTION
    { "option" }
| PLUS
    { "nonempty_list" }
| STAR
    { "list" }

/* ------------------------------------------------------------------------- */
/* A postlude is announced by %%, but is optional. */

postlude:
  EOF
    { None }




%inline plist(X):
  params = loption(delimited(LPAREN, separated_nonempty_list(COMMA, X), RPAREN))
    { (print_endline (Batteries.dump ("DEBUG:params", params)));
	params }


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

%%
