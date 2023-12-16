
(* The type of tokens. *)

type token = 
  | TERMINAL of (SentenceParserAux.raw_symbol)
  | NONTERMINAL of (SentenceParserAux.raw_symbol)
  | EOL
  | EOF
  | COMMENT of (string)
  | COLONCOLONEQUALS

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val optional_sentence: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (SentenceParserAux.raw_sentence option)

val entry: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (SentenceParserAux.located_raw_sentence SentenceParserAux.or_comment list)
