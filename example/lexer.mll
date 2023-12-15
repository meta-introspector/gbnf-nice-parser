{
open Menhir_parser

exception LexError of string

let[@inline] failwith msg = raise (LexError msg)

let[@inline] illegal c =
  failwith (Printf.sprintf "[lexer] unexpected character: '%c'" c)
}

(* regular expressions *)
let whitespace = ' ' | '\t'
let newline = "\r\n" | '\r' | '\n'
let ident = ['A'-'Z' 'a'-'z' '0'-'9' '_']*

(* ;; letter ::= "A" | "B" | "C" | "D" | "E" | "F" | "G"       | "H" | "I" | "J" | "K" | "L" | "M" | "N"       | "O" | "P" | "Q" | "R" | "S" | "T" | "U"       | "V" | "W" | "X" | "Y" | "Z" | "a" | "b"       | "c" | "d" | "e" | "f" | "g" | "h" | "i"        | "j" | "k" | "l" | "m" | "n" | "o" | "p"       | "q" | "r" | "s" | "t" | "u" | "v" | "w"       | "x" | "y" | "z"  *)

(* ;; digit ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"  *)
(* ;; # removed "  | "\f" | "\b" *)
(* ;; symbol ::= "[" | "]" | "{" | "}" | "(" | ")" | "<" | ">"  | "'" |  "=" | "|" | "." | "," | ";" | "-"        | "+" | "*" | "?" | "\n" | "\t" | "\r"   *)

(* ;; character ::= letter | digit | symbol | "_" | " "  *)
(* ;; identifier ::= letter ( letter | digit | "_" ) *)

(* ;; #| "\f" | "\b" *)
(* ;; S ::= ( " " | "\n" | "\t" | "\r"  ) *)

rule next_token = parse
  | eof { EOF }
  | whitespace+
    { next_token lexbuf }
  | newline
    { Lexing.new_line lexbuf; next_token lexbuf }
  | "(*"
    { comment 0 lexbuf; next_token lexbuf }

  (* YOUR TOKENS HERE... *)
  | '(' { LPAR }
  | ')' { RPAR }
  | '[' { LBRA }
  | ']' { RBRA }
  | '{' { LCUR }
  | '}' { RCUR }

  (* lex identifiers last, so keywords are not lexed as identifiers *)
  | ident as atom { ATOM atom }

  (* no match? raise exception *)
  | _ as c { illegal c }


(* allow nested comments, like OCaml *)
and comment nesting = parse
  | "(*"
    { comment (nesting+1) lexbuf }
  | "*)"
    { if nesting > 0 then comment (nesting - 1) lexbuf }
  | eof
    { failwith "[lexer] unterminated comment at EOF" }
  | _
    { comment nesting lexbuf }

