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

(* An injection of symbol expressions into choice expressions. *)

let inject (e : symbol_expression located) : expression =
  (print_endline (Batteries.dump ("DEBUG:inject", e)));
  Positions.pmap (fun pos e ->
    let branch =
      Branch (
          Positions.with_pos pos (ESingleton e),
          ParserAux.new_production_level()
      )
    in
    EChoice [ branch ]
  ) e

(* When a stretch has been created by [Lexer.mk_stretch] with [parenthesize]
   set to [true], it includes parentheses. In some (rare) cases, this is
   undesirable. The following function removes the parentheses a posteriori.
   They are replaced with whitespace, so as to not alter column numbers. *)

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

/* ------------------------------------------------------------------------- */
/* Type annotations and start symbol. */

(* %type <ParserAux.early_producer> producer *)
(* %type <ParserAux.early_production> production *)
%start <Syntax.partial_grammar> grammar

/* ------------------------------------------------------------------------- */
/* Priorities. */

/* These declarations solve a shift-reduce conflict in favor of shifting: when
   the right-hand side of an old-style rule begins with a leading bar, this
   bar is understood as an (insignificant) leading optional bar, *not* as an
   empty right-hand side followed by a bar. This ambiguity arises due to the
   possibility for several productions to share a single semantic action.
   The new rule syntax does not have this possibility, and has no ambiguity. */

%nonassoc no_optional_bar
%nonassoc BAR

/* ------------------------------------------------------------------------- */
/* On-error-reduce declarations. */

/* These declarations reduce the number of states where an error can occur,
   thus reduce the number of syntax error messages that we have to write in
   parserMessages.messages. */

%on_error_reduce old_rule


%on_error_reduce separated_nonempty_list(COMMA,symbol)
%on_error_reduce separated_nonempty_list(COMMA,pattern)

%on_error_reduce loption(delimited(LPAREN,separated_nonempty_list(COMMA,expression),RPAREN))

%%

/* ------------------------------------------------------------------------- */
/* A grammar consists of  rules */

grammar:
  rs = old_rule*
    {
      (* (print_endline (Batteries.dump ("DEBUG:rs",rs))); *)
      { 
        pg_filename          = ""; (* filled in by the caller *)           
        pg_rules             = rs;
      (*   pg_postlude = None; *)
      (*   pg_declarations = []; *)
      } 
    }


/* This production recognizes tokens that are valid in the rules section,
   but not in the declarations section. This is a hint that a %% was
   forgotten. */

| rule_specific_token
    {
      Error.error [Positions.import $loc]
        "syntax error inside a declaration.\n\
         Did you perhaps forget the %%%% that separates declarations and rules?"
    }

(* priority_keyword: *)
(*   LEFT *)
(*     { LeftAssoc } *)
(* | RIGHT *)
(*     { RightAssoc } *)
(* (\* | NONASSOC *\) *)
(* (\*     { NonAssoc } *\) *)

%inline rule_specific_token:
| PUBLIC
| INLINE
| COLON
| LET
| EOF
    { () }

/* ------------------------------------------------------------------------- */
/* Our lists of symbols are separated with optional commas. Order is
   irrelevant. */

%inline clist(X):
  xs = separated_nonempty_list(COMMA?, X)
    { xs }

/* ------------------------------------------------------------------------- */
/* A symbol is a terminal or nonterminal symbol. */

/* One would like to require nonterminal symbols to begin with a lowercase
   letter, so as to lexically distinguish them from terminal symbols, which
   must begin with an uppercase letter. However, for compatibility with
   ocamlyacc, this is impossible. It can be required only for nonterminal
   symbols that are also start symbols. */

/* We also accept token aliases in place of ordinary terminal symbols.
   Token aliases are quoted strings. */

symbol:
  id = LID
| id = UID
| id = QID
    {
      (print_endline (Batteries.dump ("DEBUG:ID", id)));
      id }

/* ------------------------------------------------------------------------- */
/* Terminals must begin with an uppercase letter. Nonterminals that are
   declared to be start symbols must begin with a lowercase letter. */

/* In declarations, terminals must be UIDs, but we may also declare
   token aliases, which are QIDs. */

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

/* ------------------------------------------------------------------------- */
/* A rule defines a symbol. It is optionally declared %public, and optionally
   carries a number of formal parameters. The right-hand side of the definition
   consists of a list of productions. */

old_rule:
  flags = flags            /* flags */
  symbol = symbol          /* the symbol that is being defined */
COLONCOLONEQUAL
branches = branches
NEWLINE
	       {
		 (print_endline (Batteries.dump ("DEBUG:branches", branches)));
                 {
                   pr_nt          = Positions.value symbol;
                   pr_positions   = [ Positions.position symbol ];
                   pr_branches    =  branches
                 }
    }

%inline branches:
  prods = separated_nonempty_list(BAR, production_group)
    {
      (print_endline (Batteries.dump ("DEBUG:branches",prods)));
      [](* prods *)
    }

flags:
  /* epsilon */
    { false, false }
| PUBLIC
    { true, false }
| INLINE
    { false, true }
| PUBLIC INLINE
| INLINE PUBLIC
    { true, true }

optional_bar:
  /* epsilon */ %prec no_optional_bar
| BAR
    { () }

/* ------------------------------------------------------------------------- */
/* A production group consists of a list of productions, followed by a
   semantic action and an optional precedence specification. */

production_group:
  productions = separated_nonempty_list(BAR, production)
                  (* action = ACTION *)

    {
      (print_endline (Batteries.dump ("DEBUG:production_group", productions)));
      (* If multiple productions share a single semantic action, check
         that all of them bind the same names. *)
      (* ParserAux.check_production_group productions; *)
      (* Then, *)
      (* List.map (fun (producers, oprec1, level, pos) -> *)
      (*   (\* Replace [$i] with [_i]. *\) *)
      (*   let pr_producers =  producers in *)
      (*   (\* Distribute the semantic action. Also, check that every [$i] *)
      (*      is within bounds. *\) *)
      (*   let names = producers in *)
      (*   (\*let pr_action = action Settings.dollars names in*\) *)
      (*   { *)
      (*     pr_producers; *)
      (*     pr_action = Action.from_il_expr (EVar "None"); *)
      (*     pr_branch_prec_annotation   = ParserAux.override pos oprec1 None; *)
      (*     pr_branch_production_level  = level; *)
      (*     pr_branch_position          = pos *)
      (*   }) *)
      productions
      
    }


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


/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

/* The new rule syntax. */

/* Whereas the old rule syntax allows a nonterminal symbol to begin with an
   uppercase letter, the new rule syntax disallows it. The left-hand side of a
   new rule must be a lowercase identifier [LID]. */

/* A new rule *cannot* be terminated by a semicolon. (This is contrast with a
   traditional rule, which can be followed with any number of semicolons.) We
   are forced to forbid the use of semicolons are as a rule terminator because
   they are used already as a sequencing construct. Permitting both uses would
   give rise to a shift/reduce conflict that we would not be able to solve. */

new_rule:
| rule_public     = boption(PUBLIC)
  LET
  rule_lhs        = LID
  rule_formals    = plist(symbol)
  rule_inline     = equality_symbol
  rule_rhs        = expression
NEWLINE
    {
      (print_endline (Batteries.dump ("DEBUG:new_rule", rule_lhs)));
      {
       rule_public;
       rule_inline;
       rule_lhs;
       (* rule_attributes; *)
       rule_formals;
       rule_rhs;
    }}

/* A new rule is written [let foo := ...] or [let foo == ...].
   In the former case, we get an ordinary nonterminal symbol;
   in the latter case, we get an %inline nonterminal symbol. */

equality_symbol:
  COLONEQUAL
    { false }
| EQUALEQUAL
    { true  }

/* The right-hand side of a new rule is an expression. */

/* An expression is a choice expression. */

expression:
  e = located(choice_expression)
    { e }

/* A choice expression is a bar-separated list of alternatives, with an
   optional leading bar, which is ignored. Each alternative is a sequence
   expression. */

/* We cannot allow a choice expression to be empty, even though that would
   make semantic sense (the empty sum is void). Indeed, that would create a
   shift/reduce conflict: after reading [def x = y], it would be unclear
   whether this is a definition of [x] as an alias for [y], or a definition of
   [x] as an alias for the empty sum, followed with an old-style rule that
   happens to begin with [y]. */

%inline choice_expression:
  branches = preceded_or_separated_nonempty_llist(BAR, branch)
    { EChoice branches }

%inline branch:
  e = seq_expression
    {
      (print_endline (Batteries.dump ("DEBUG:branch", e)));
      Branch (e, ParserAux.new_production_level()) }

/* A sequence expression takes one of the following forms:

         e1; e2     a sequence that binds no variables (sugar for _ = e1; e2)
     p = e1; e2     a sequence that binds the variables in the pattern p

   or is an symbol expression or an action expression. */

/* Allowing an symbol expression [e] where a sequence expression is expected
   can be understood as syntactic sugar for [x = e; { x }]. */

/* In a sequence [e1; e2] or [p = e1; e2], the left-hand expression [e1] is
   *not* allowed to be an action expression. That would be a Bison-style
   midrule action. Instead, one must explicitly write [midrule({ ... })]. */

/* In a sequence, the semicolon cannot be omitted. This is in contrast with
   old-style rules, where semicolons are optional. Here, semicolons are
   required for disambiguation: indeed, in the absence of mandatory
   semicolons, when a sequence begins with x(y,z), it would be unclear whether
   1- x is a parameterized symbol and (y,z) are its actual arguments, or 2- x
   is unparameterized and (y, z) is a tuple pattern which forms the beginning
   of the next element of the sequence. */

/* We *could* allow the semicolon to be omitted when it precedes an action
   expression (as opposed to a sequence expression). This would be implemented
   in the definition of the nonterminal symbol [continuation]. We choose not
   to do this, as we wish to make it clear in this case that this is a
   sequence whose last element is the action expression. */

%inline seq_expression:
  e = located(raw_seq_expression)
    { e }

raw_seq_expression:
|                    e1 = symbol_expression e2 = continuation
    { ECons (SemPatWildcard, e1, e2) }
| p1 = pattern EQUAL e1 = symbol_expression e2 = continuation
    { ECons (p1, e1, e2) }
| e = symbol_expression
    { ESingleton e }

%inline continuation:
  SEMI e2 = seq_expression
/* |   e2 = action_expression */
    { e2 }

/* A symbol expression takes one of the following forms:

     foo(...)       a terminal or nonterminal symbol (with parameters)
     e*             same as above
     e+             same as above
     e?             same as above */

/* Note the absence of parenthesized expressions [(e)] in the syntax of symbol
   expressions. There are two reasons why they are omitted. At the syntactic
   level, introducing them would create a conflict. At a semantic level, they
   are both unnecessary and ambiguous, as one can instead write [endrule(e)]
   or [midrule(e)] and thereby indicate whether the anonymous nonterminal
   symbol that is generated should or should not be marked %inline. */

symbol_expression:
| symbol = symbol es = plist(expression) 
    { ESymbol (symbol, es ,[]) }
| e = located(symbol_expression) m = located(modifier) 
    (* We are forced by syntactic considerations to require a symbol expression
       in a position where an expression is expected. As a result, an injection
       must be applied. *)
    { ESymbol (m, [ inject e ], []) }


/* Patterns. */

pattern:
| x = LID
    { SemPatVar x }
| UNDERSCORE
    { SemPatWildcard }
| TILDE
    { SemPatTilde (Positions.import $loc) }
| LPAREN ps = separated_list(COMMA, pattern) RPAREN
    { SemPatTuple ps }

(* -------------------------------------------------------------------------- *)

(* Generic definitions. *)

(* ------------------------------------------------------------------------- *)


%inline plist(X):
  params = loption(delimited(LPAREN, separated_nonempty_list(COMMA, X), RPAREN))
    { (print_endline (Batteries.dump ("DEBUG:params", params)));
	params }

(* -------------------------------------------------------------------------- *)

(* [reversed_preceded_or_separated_nonempty_llist(delimiter, X)] recognizes a
   nonempty list of [X]s, separated with [delimiter]s, and optionally preceded
   with a leading [delimiter]. It produces an OCaml list in reverse order. Its
   definition is left-recursive. *)

reversed_preceded_or_separated_nonempty_llist(delimiter, X):
| ioption(delimiter) x = X
    { [x] }
| xs = reversed_preceded_or_separated_nonempty_llist(delimiter, X)
  delimiter
  x = X
    { x :: xs }

(* [preceded_or_separated_nonempty_llist(delimiter, X)] recognizes a nonempty
   list of [X]s, separated with [delimiter]s, and optionally preceded with a
   leading [delimiter]. It produces an OCaml list in direct order. *)

%inline preceded_or_separated_nonempty_llist(delimiter, X):
  xs = rev(reversed_preceded_or_separated_nonempty_llist(delimiter, X))
    { xs }

(* [preceded_or_separated_llist(delimiter, X)] recognizes a possibly empty
   list of [X]s, separated with [delimiter]s, and optionally preceded with a
   leading [delimiter]. It produces an OCaml list in direct order. *)

preceded_or_separated_llist(delimiter, X):
| (* empty *)
    { [] }
| xs = preceded_or_separated_nonempty_llist(delimiter, X)
    { xs }

(* -------------------------------------------------------------------------- *)

(* [located(X)] recognizes the same language as [X] and converts the resulting
   value from type ['a] to type ['a located]. *)

located(X):
  x = X
    { with_loc $loc x }

%%
