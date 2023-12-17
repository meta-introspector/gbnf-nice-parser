(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

(* The front-end. This module performs a series of toplevel side effects. *)

(* ------------------------------------------------------------------------- *)

(* Reading a grammar from a file. *)

let load_grammar_from_contents filename contents =
  InputFile.new_input_file filename;
  InputFile.with_file_contents contents (fun () ->
    let open Lexing in
    let lexbuf = Lexing.from_string contents in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    (* the grammar: *)
    (* { (Driver.grammar Lexer.main lexbuf) *)
    (*   with Syntax.pg_filename = filename } *)
  )



(* ------------------------------------------------------------------------- *)

(* Read all of the grammar files that are named on the command line, plus the
   standard library, unless suppressed by [--no-stdlib] or [--coq]. *)


(* ------------------------------------------------------------------------- *)

(* Eliminate anonymous rules. *)

let grammars =()

(* ------------------------------------------------------------------------- *)

(* If several grammar files were specified, merge them. *)

let grammar =()

(* ------------------------------------------------------------------------- *)

(* Check that the grammar is well-sorted; infer the sort of every symbol. *)

let sorts =()

(* ------------------------------------------------------------------------- *)

(* Expand away all applications of parameterized nonterminal symbols, so as
   to obtain a grammar without parameterized nonterminal symbols. *)


(* ------------------------------------------------------------------------- *)

(* If [--only-tokens] was specified on the command line, produce
   the definition of the [token] type and stop. *)


(* ------------------------------------------------------------------------- *)

(* Perform reachability analysis. *)


(* ------------------------------------------------------------------------- *)

(* If [--infer] was specified on the command line, perform type inference.
   The OCaml type of every nonterminal symbol is then known. *)

(* If [--depend] or [--raw-depend] was specified on the command line,
   perform dependency analysis and stop. *)

(* The purpose of [--depend] and [--raw-depend] is to support [--infer].
   Indeed, [--infer] is implemented by producing a mock [.ml] file (which
   contains just the semantic actions) and invoking [ocamlc]. This requires
   certain [.cmi] files to exist. So, [--(raw-)depend] is a way for us to
   announce which [.cmi] files we need. It is implemented by producing the
   mock [.ml] file and running [ocamldep] on it. We also produce a mock
   [.mli] file, even though in principle it should be unnecessary -- see
   comment in [nonterminalType.mli]. *)

(* If [--infer-write-query] was specified on the command line, write a
   mock [.ml] file and stop. It is then up to the user (or build system)
   to invoke [ocamlc -i] on this file, so as to do type inference. *)

(* If [--infer-read-reply] was specified on the command line, read the
   inferred [.mli] file. The OCaml type of every nonterminal symbol is
   then known, just as with [--infer]. *)


(* ------------------------------------------------------------------------- *)

(* Expand away some of the position keywords. *)


(* If [--no-inline] was specified on the command line, skip the
   inlining of non terminal definitions marked with %inline. *)

(* ------------------------------------------------------------------------- *)

(* If [--only-preprocess] or [--only-preprocess-drop] was specified on the
   command line, print the grammar and stop. Otherwise, continue. *)
