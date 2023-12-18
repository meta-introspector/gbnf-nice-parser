open Gbnf_parser

(* let open Segment in *)
 (* let c = Error.new_category() in *)
 (* Read and segment the file. *)
 (* let segments : (tag * string * Lexing.lexbuf) list = segment filename in *)
 (* Process the segments, two by two. We expect one segment to contain
   a non-empty series of sentences, and the next segment to contain
   free-form text. *)
 (* let rec loop accu segments = *)
 (*  match segments with *)
 (*  | [] -> *)
 (*    List.rev accu *)
 (*  | (Whitespace, comments, _) :: segments -> *)
 (*    loop (mkcomment comments accu) segments *)
 (*  | (Segment, _, lexbuf) :: segments -> *)
(*    (\* Read a series of located sentences. *\) *)


let () =
 let inputstr = {codesample|
letter ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" 
digit ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" 
symbol ::= "[" | "]" | "{" | "}" | "(" | ")" | "<" | ">" | "'" | "=" | "|" | "." | "," | ";" | "-" | "+" | "*" | "?" | "\n" | "\t" | "\r" 
character ::= letter | digit | symbol | "_" | " " 
identifier ::= letter ( letter | digit | "_" )
S ::= ( " " | "\n" | "\t" | "\r" )
terminal ::= "'" character "'" ( character "'" ) "'"
terminator ::= (";" | ".")
term ::= "(" S rhs S ")" | "[" S rhs S "]" | "{" S rhs S "}" | terminal | identifier 
factor ::= term S "?" | term S "*" | term S "+" | term S "-" S term | term S 
concatenation ::= ( S factor S "," ? ) + 
alternation ::= ( S concatenation S "|" ? ) + 
rhs ::= alternation 
lhs ::= identifier 
rule ::= lhs S "=" S rhs S terminator 
root ::= ( S rule S ) * 

                 |codesample} in

 let lexbuf = Lexing.from_string inputstr in
 match SentenceParser.entry SentenceLexer.lex lexbuf with
 | exception Parsing.Parse_error ->
 Error.error
 [Positions.cpos lexbuf]
 "ill-formed sentence."
 | elements ->
 (print_endline (Batteries.dump elements));
 | exception e ->
 let msg = Printexc.to_string e in
 let curr = lexbuf.Lexing.lex_curr_p in
 let line = curr.Lexing.pos_lnum in
 let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
 print_endline (Batteries.dump ["error",e, msg, lexbuf, line,cnum])
 ;
 
