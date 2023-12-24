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

/*  Copyright 2023 James Michael Dupont */
/*  
Starting with the menhir sentence parser, replacing with stage2 parser, adding in wikipedias DFA and parts of bnfgen
 https://github.com/dmbaturin/bnfgen
and ocaml code itself ocaml/lex/parser.mly 
attempt to parse the gbnf.
 */
/*
grammar
  rules:
    old_rule: lid ::= rhs
    rsa alt concat


*/
/* ------------------------------------------------------------------------- */
/* Imports. */

%{

open Stretch
open Syntax


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


%}

/* ------------------------------------------------------------------------- */
/* Tokens. */

%token <int> Tchar
%token DASH "-"
%token CARET "^"
%token
  BAR              "|"
  EOF              ""
  LPAREN           "("
  RPAREN           ")"
  LBRACE "["
  RBRACE "]"
  COMMA            ","
  QUESTION         "?"
  STAR             "*"
  PLUS             "+"
NEWLINE

%token <string Positions.located>
  LID              "lident"
  QID              "\"alias\""

%left      left
%right     right
%nonassoc  nonassoc

/* For the new rule syntax: */
%token
   COLONCOLONEQUAL  "::="

(* %type <ParserAux.early_producer> producer *)
(* %type <ParserAux.early_production> production *)
%start <Syntax.partial_grammar> grammar



%%

/* ------------------------------------------------------------------------- */
/* A grammar consists of  rules 
taken from https://github.com/dmbaturin/bnfgen
*/
rules:
separated_nonempty_list(NEWLINE+, old_rule)  {
			 (print_endline (Batteries.dump ("DEBUG:OLDRULE",$1)))
		       } 


grammar:
  rs =  rules postlude
    {
      (print_endline (Batteries.dump ("DEBUG:rs",rs)));
      {
        pg_filename          = ""; (* filled in by the caller *)
        pg_rules             = [];
      }
    }

/* rule_specific_token: */
/* | COLON */
/* | EOF */
/*     { () } */


 clist(X):
  xs = separated_nonempty_list(COMMA?, X)
    { xs }


/* symbol: */
/* id = LID */
/*     { */
/*       (print_endline (Batteries.dump ("DEBUG:LID", id))); */
/*       id } */
/*   | id = QID */
/*     { */
/*       (print_endline (Batteries.dump ("DEBUG:QID", id))); */
/*       id } */

old_rule:
symbol = LID
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

 preceded_or_separated_nonempty_llist(delimiter, X):
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

%inline qid:
  | QID {}
%inline lid:
  | LID {}

%inline sterm:
  | qid {}
  | lid {}

term:
  | complexterms {} 
  | sterm {}

%inline  complexterms: 
   | group1 {} 
   | class1  {} 

%inline  group1: 
  | LPAREN rhs  RPAREN {}

%inline class1: 
  | LBRACE char_class  RBRACE {}

%inline termfactor:
  | term   {}

factor:
  | termfactor modifier {}
  | termfactor  {}

%inline modifier:
  | fplus {}
  | fquest {}
  | fstar {}

%inline fstar:
  |  STAR {}
%inline fquest:
  |  QUESTION {}
%inline fplus:
  | PLUS {}


%inline fconcatenation:
  | factor  {}
  /* | factor modifier {} */

concatenation:
  | concatenation factor  {}
  | factor {}

alternation:
  | alternation BAR concatenation
  | concatenation {}

rhs:
  | alternation {}




/* ===EBNF=== */

/* from wikipedia Even EBNF can be described using EBNF. Consider below grammar (using conventions such as "-" to indicate set disjunction, "+" to indicate one or more matches, and "?" for optionality): */

/* <syntaxhighlight lang="ebnf"> */
/* letter = "A" | "B" | "C" | "D" | "E" | "F" | "G" */

/* digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ; */

/* symbol = "[" | "]" | "{" | "}" | "(" | ")" | "<" | ">" */
/*        | "'" | '"' | "=" | "|" | "." | "," | ";" | "-"  */
/*        | "+" | "*" | "?" | "\n" | "\t" | "\r" | "\f" | "\b" ; */

/* character = letter | digit | symbol | "_" | " " ; */
/* identifier = letter , { letter | digit | "_" } ; */

/* S = { " " | "\n" | "\t" | "\r" | "\f" | "\b" } ; */

/* terminal = "'" , character - "'" , { character - "'" } , "'" */
/*          | '"' , character - '"' , { character - '"' } , '"' ; */

/* terminator = ";" | "." ; */

/* term = "(" , S , rhs , S , ")" */
/*      | "[" , S , rhs , S , "]" */
/*      | "{" , S , rhs , S , "}" */
/*      | terminal */
/*      | identifier ; */

/* factor = term , S , "?" */
/*        | term , S , "*" */
/*        | term , S , "+" */
/*        | term , S , "-" , S , term */
/*        | term , S ; */

/* concatenation = ( S , factor , S , "," ? ) + ; */
/* alternation = ( S , concatenation , S , "|" ? ) + ; */

/* rhs = alternation ; */
/* lhs = identifier ; */

/* rule = lhs , S , "=" , S , rhs , S , terminator ; */

/* grammar = ( S , rule , S ) * ; */

/* </syntaxhighlight> */


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
  /* | char_class1 char_class1  CONCAT */
  /*       { Cset.union $1 $2 } */
;



%%
