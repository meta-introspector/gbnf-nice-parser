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

%{
open Syntax
%}

%token <int> Tchar
%token DASH "-"
%token CARET "^"
%token
  BAR              "|"
  EOF              ""
  LPAREN           "("
  RPAREN           ")" 
  QUESTION         "?"
  STAR             "*"
  PLUS             "+"
NEWLINE

%token <string Positions.located>
   LID              "lident"
   REGEX            "regex"
   QID              "\"alias\""

%token
   COLONCOLONEQUAL  "::="

%start <Syntax.partial_grammar> grammar

%%

rules:
separated_nonempty_list(NEWLINE+, rule)  {
			 (print_endline (Batteries.dump ("DEBUG:OLDRULE",$1)))
		       } 

grammar:
  rs =  NEWLINE* rules NEWLINE* postlude
    {
      (print_endline (Batteries.dump ("DEBUG:rs",rs)));
      {
        pg_filename          = ""; (* filled in by the caller *)
        pg_rules             = [];
      }
    }

rule:
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
 | LPAREN NEWLINE* rhs  RPAREN {} 

%inline class1: 
/* | LBRACE char_class  RBRACE {} */
  |  char_class   {}
  |  REGEX {}

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

concatenation:
  | concatenation factor  {}
  | factor {}

alternation:
  | alternation BAR NEWLINE* concatenation
  | concatenation {}

rhs:
  | alternation {}


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
  | char_class1 Tchar
    /* Cset.singleton $1 */
    {   (print_endline (Batteries.dump ("DEBUG:rs",$1))) }
  | Tchar
    /* Cset.singleton $1 */
    {   (print_endline (Batteries.dump ("DEBUG:rs",$1))) }
  /* | char_class1 char_class1  CONCAT */
  /*       { Cset.union $1 $2 } */
;

%%
