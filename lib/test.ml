  open Lexing


let keep_lexeme_start lexbuf f =
  let start_p = lexeme_start_p lexbuf in
  let x = f () in
  lexbuf.lex_start_p <- start_p;
  x
