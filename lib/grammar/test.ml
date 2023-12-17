
let read_messages   =
  (* let open Segment in *)
  (* let c = Error.new_category() in *)
  (* Read and segment the file. *)
  (* let segments : (tag * string * Lexing.lexbuf) list = segment filename in *)
  (* Process the segments, two by two. We expect one segment to contain
     a non-empty series of sentences, and the next segment to contain
     free-form text. *)
  (* let rec loop accu segments = *)
  (*   match segments with *)
  (*   | [] -> *)
  (*       List.rev accu *)
  (*   | (Whitespace, comments, _) :: segments -> *)
  (*       loop (mkcomment comments accu) segments *)
  (*   | (Segment, _, lexbuf) :: segments -> *)
  (*       (\* Read a series of located sentences. *\) *)
  let lexbuf = Lexing.from_string {codesample|
root  ::= (expr "=" ws term "\n")+
expr  ::= term ([-+*/] term)*
term  ::= ident | num | "(" ws expr ")" ws
ident ::= [a-z] [a-z0-9_]* ws
num   ::= [0-9]+ ws
ws    ::= [ \t\n]*
|codesample} in
  (* let lexbuf =  Lexing.lexbuf "Hello world " in *)
  match SentenceParser.entry SentenceLexer.lex lexbuf with
  | exception Parsing.Parse_error ->
     Error.error
       [Positions.cpos lexbuf]
       "ill-formed sentence."
  | elements ->
     Batteries.dump elements

     (* [elements] is a list of located raw sentences or comments.
        Validate it. Any sentences that do not pass validation are
               removed (and error messages are emitted). In an effort to
               be robust, we continue. If there remain zero sentences,
               then this entry is removed entirely. *)
     (* validate_entry c  *)
     (* let elements = elements in *)
            (* In principle, we should now find a segment of whitespace
               followed with a segment of text. By construction, the two
               kinds of segments alternate. *)
  (*    match segments with *)
  (*           | (Whitespace, delimiter, _) :: *)
  (*             (Segment, message, _) :: *)
  (*             segments -> *)
  (*               if count_things elements = 0 then *)
  (*                 (\* There remain zero sentences. Skip this entry. *\) *)
  (*                 loop accu segments *)
  (*               else *)
  (*                 (\* Accumulate this entry. *\) *)
  (*                 let run = { elements; delimiter; message } in *)
  (*                 loop (Thing run :: accu) segments *)
  (*           | [] *)
  (*           | [ _ ] -> *)
  (*               Error.error *)
  (*                 (Positions.one (Lexing.lexeme_end_p lexbuf)) *)

  (*           | (Segment, _, _) :: _ *)
  (*           | (Whitespace, _, _) :: (Whitespace, _, _) :: _ -> *)
  (*               (\* Should not happen, thanks to the alternation between the *)
  (*                  two kinds of segments. *\) *)
  (*               assert false *)
  (* in *)
  (* let runs = stats (loop [] segments) in *)
  (* if strict then Error.exit_if c; *)
  (* runs *)


(* let process (line : string) = *)
(*   let linebuf = Lexing.from_string line in *)
(*   try *)
(*     (\* Run the parser on this line of input. *\) *)

(*   with *)
(*   | Lexer.Error msg -> *)
(*   | Parser.Error -> *)
(* let process (optional_line : string option) = *)
(*   match optional_line with *)
(*   | None -> *)
(*       () *)
(*   | Some line -> *)
(*       process line *)

(* let rec repeat channel = *)
(*   (\* Attempt to read one line. *\) *)
(*   let optional_line, continue = SentenceLexer.lex channel in *)
(*   process optional_line; *)
(*   if continue then *)
(*     repeat channel *)
  
(* let () = *)
(*   repeat (Lexing.from_channel stdin) *)

