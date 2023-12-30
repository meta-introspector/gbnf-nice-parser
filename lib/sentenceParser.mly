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
%type <Syntax.myfactor>  factor
%type <Syntax.myfactor>  alternation
%type <Syntax.myfactor>  modifier
%type <Syntax.myfactor>  complexterms
%type <Syntax.myfactor>  term
%type <Syntax.myfactor>  fstar
%type <Syntax.myfactor>  sterm
%type <Syntax.myfactor>  char_class
%type <string Positions.located>  lid
%type <string Positions.located>  qid
%type <Syntax.myfactor>  termfactor

%%

rules:
separated_nonempty_list(NEWLINE+, rule)  {
			 (print_endline (Batteries.dump ("DEBUG:OLDRULE",$1)))
		       } 

grammar:
  rs =  NEWLINE* rules NEWLINE* postlude
    {
      (print_endline (Batteries.dump ("DEBUG:grammar",rs, $2)));
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
      (print_endline (Batteries.dump ("DEBUG:rule", symbol, branches)));
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
  | QID {      (print_endline (Batteries.dump ("DEBUG:quid", $1)));  $1 }
%inline lid:
  | LID {      (print_endline (Batteries.dump ("DEBUG:lid", $1)));  $1 }

%inline sterm:
  | qid {      (print_endline (Batteries.dump ("DEBUG:sterm/quid", $1))); SFactor $1}
  | lid {      (print_endline (Batteries.dump ("DEBUG:sterm/lid", $1))); SFactor $1}

term:
  | complexterms {      (print_endline (Batteries.dump ("DEBUG:term/cterms", $1))); NFactor $1}
  | sterm {      (print_endline (Batteries.dump ("DEBUG:term/sterm", $1))); NFactor $1}

%inline  complexterms: 
  | group1 {      (print_endline (Batteries.dump ("DEBUG:cterm/group", $1))); NFactor $1}
  | class1  {      (print_endline (Batteries.dump ("DEBUG:cterm/class", $1))); NFactor $1}

%inline  group1: 
 | LPAREN NEWLINE* rhs  RPAREN {      (print_endline (Batteries.dump ("DEBUG:rhs", $3))); NFactor $3} 

%inline class1: 
/* | LBRACE char_class  RBRACE {} */
  |  char_class   {      (print_endline (Batteries.dump ("DEBUG:class1a", $1))); NFactor $1}
  |  REGEX {      (print_endline (Batteries.dump ("DEBUG:class", $1))); SFactor $1}

%inline termfactor:
  | term   {      (print_endline (Batteries.dump ("DEBUG:termfactor", $1))); NFactor $1}

factor:
  | termfactor modifier {
		 (* (print_endline (Batteries.dump ("DEBUG:factormod", ($1,$2)))); *)
		 (* let foo = CFactor ($1, $2) *)
	(* in foo 
           (CFactor ($1, $2) )
         *)

        NFactor $1
      }

  | termfactor  {
	(* (print_endline (Batteries.dump ("DEBUG:factor", $1))); *)
	(* let foo = SFactor $1 in *)
	(* foo *)
        NFactor $1
      }

%inline modifier:
  | fplus {      (print_endline (Batteries.dump ("DEBUG:mod", $1))); NFactor $1}
  | fquest {      (print_endline (Batteries.dump ("DEBUG:quest", $1))); NFactor $1}
  | fstar {      (print_endline (Batteries.dump ("DEBUG:star", $1))); NFactor $1}

%inline fstar:
   |  STAR {
          (* (print_endline (Batteries.dump ("DEBUG:star", $1))); *)
          Star
        }
%inline fquest:
  |  QUESTION {      (print_endline (Batteries.dump ("DEBUG:quest", $1))); Question}
%inline fplus:
  | PLUS {      (print_endline (Batteries.dump ("DEBUG:plus", $1))); Plus}

concatenation:
  | concatenation factor  {      (print_endline (Batteries.dump ("DEBUG:concat1", $1))); NFactor $1}
  | factor {      (print_endline (Batteries.dump ("DEBUG:concat2", $1))); NFactor $1}

alternation:
  | alternation BAR NEWLINE* concatenation { NFactor  $1 }
  | concatenation {      (print_endline (Batteries.dump ("DEBUG:alt", $1))); NFactor $1}

rhs:
  | alternation {      (print_endline (Batteries.dump ("DEBUG:rhs", $1))); NFactor $1}


char_class:
    CARET char_class1
    /* { Cset.complement $2 } */
{   (print_endline (Batteries.dump ("DEBUG:ccrs",$2))) ; NFactor $2}
  | char_class1
    /* { $1 } */
    {   (print_endline (Batteries.dump ("DEBUG:cc2rs",$1))); CharClass }
;
char_class1:
    Tchar DASH Tchar
    /* { Cset.interval $1 $3 } */
         {   (print_endline (Batteries.dump ("DEBUG:cc3rs",$1,$2)));  CharInt $1
                                                                                 (*fixme*)
         }
  | char_class1 Tchar
    /* Cset.singleton $1 */
    {   (print_endline (Batteries.dump ("DEBUG:cc4rs",$1))); NFactor $1 }
  | Tchar
    /* Cset.singleton $1 */
    {   (print_endline (Batteries.dump ("DEBUG:cc5rs",$1))); CharInt $1 }
  /* | char_class1 char_class1  CONCAT */
  /*       { Cset.union $1 $2 } */
;

%%
