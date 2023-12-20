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

/* This is two parsers in one. */

/* This parser is used to read the sentences provided on the standard input
   channel when [--interpret] is set. The entry point is [optional_sentence]. */

/* It is used also to read a [.messages] file. The entry point is [entry]. */

/* This parser must be compatible with both ocamlyacc and menhir, so we use
   $ notation, do not use Menhir's standard library, and collect positions
   manually. */

/* ------------------------------------------------------------------------ */
/* Tokens. COLON EQUALS*/

%token  COLONCOLONEQUALS EOF EOL
%token LBRACE RBRACE
%token <string> IDENT
%token LCURLY RCURLY
%token PIPE GT LT MINUS
%token LPAREN RPAREN COMMA DOT NULL 
%token<SentenceParserAux.raw_symbol> TERMINAL
%token<SentenceParserAux.raw_symbol> NONTERMINAL
%token<string> COMMENT
  /* only manually-written comments, beginning with a single # */

/* ------------------------------------------------------------------------ */
/* Types. */

%{

  open SentenceParserAux

  (* Computing the start and end positions of a sentence. *)

  let locate_sentence (nto, terminals) =
    let opening =
      match nto, terminals with
      | Some (_, opening, _), _
      | None, (_, opening, _) :: _ ->
          opening
      | None, [] ->
          Lexing.dummy_pos (* cannot happen *)
    and closing =
      match nto, List.rev terminals with
      | _, (_, _, closing) :: _
      | Some (_, _, closing), _ ->
          closing
      | None, [] ->
          Lexing.dummy_pos (* cannot happen *)
    in
    [Positions.import (opening, closing)],
    (nto, terminals)

%}

%type <raw_sentence> sentence

%type <located_raw_sentence> located_sentence

%type <SentenceParserAux.raw_sentence option> optional_sentence

%type<SentenceParserAux.located_raw_sentence SentenceParserAux.or_comment list> entry

%start optional_sentence
%start entry

%%

/* ------------------------------------------------------------------------ */

/* An entry is a list of located sentences or comments. */
entry: located_sentences_or_comments EOF
  { $1 }

/* A list of located sentences or comments. */
located_sentences_or_comments:
  { [] }
| located_sentence located_sentences_or_comments { Thing   $1 :: $2 }
| COMMENT          located_sentences_or_comments { Comment $1 :: $2 }

/* A located sentence. */
located_sentence: sentence
    { (print_endline (Batteries.dump ("sentence", $1)));
      locate_sentence $1 }

/* An optional sentence. */
optional_sentence:
| EOF
    { None }
| sentence
    {
      (print_endline (Batteries.dump  ("optional_sentence" , $1)));
      Some $1 }

/* A sentence is a pair of an optional non-terminal start symbol and a list
   of terminal symbols. It is terminated by a newline. */
production_groups:
  /* epsilon */
    { [] }
| production_groups BAR production_group
    { $3 :: $1 }

sentence:
| NONTERMINAL COLONCOLONEQUALS terminals EOL
    {
      (print_endline (Batteries.dump ("nonterminal", $1)));
      (print_endline (Batteries.dump ("terminals1" ,  $3)));
      Some $1, $3
    }
| terminals EOL
    { None, $1 }

/* A list of terminal symbols. */
terminals:
|
    { (print_endline "EMPTY"); []       }
| TERMINAL terminals /* (Lexer.extract_string_from_terminal */
    {
      (print_endline "TERMINAL terminals");
      (print_endline (Batteries.dump  ("terminal",$1)));      
      (print_endline (Batteries.dump ("terminals2",$2) ));
      []
    }
| IDENT terminals /*for quoted tokens*/
    {
      (print_endline "IDENT terminals");
      (print_endline (Batteries.dump ("ident", $1)));
      (print_endline (Batteries.dump ("terminals3", $2)));
      [];
    } 
    /* { $1 :: $2 } */
