open Gbnf_parser
let () = print_endline "Hello, World!"

let grammar_from_channel ic =
  let lexbuf = Lexing.from_channel ic in
  let buffer, lexer = MenhirLib.ErrorReports.wrap SentenceLexer.main  in
  let dat = (SentenceParser.grammar lexer lexbuf) in 
  (print_endline (Batteries.dump dat))

let grammar_from_file filename =
  let ic = open_in filename in
  let g = grammar_from_channel ic in
  let () = close_in ic in
  g

let () =
  let filename = Sys.argv.(1) in 
  let g = grammar_from_file filename in
  (print_endline (Batteries.dump g))
