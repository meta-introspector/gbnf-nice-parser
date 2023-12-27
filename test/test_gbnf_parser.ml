open Gbnf_parser
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil

(* open Gbnf_parser.SentenceParser *)


(* .MenhirBasics.Error *)
(* include MenhirBasics *)

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



(* let () = *)
(*  let lexbuf = Lexing.from_string inputstr in *)
(*  match SentenceParser.optional_sentence SentenceLexer.lex lexbuf with *)
(*     | exception Parsing.Parse_error -> *)
(*        Error.error *)
(*          [Positions.cpos lexbuf] *)
(*          "ill-formed sentence." *)
(*     | elements ->    (print_endline (Batteries.dump elements));  *)
(*     | exception e -> *)
(*        let stacktrace = Printexc.get_raw_backtrace () in *)
(*        let msg = Printexc.to_string e in *)
(*        let curr = lexbuf.Lexing.lex_curr_p in *)
(*        let line = curr.Lexing.pos_lnum in *)
(*        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in *)
(*        print_endline (Batteries.dump ["error",e, *)
(*                                       "stack", stacktrace, *)
(*                                       "msg",msg, "line",line,"cnum",cnum,"tett",lexbuf]); *)
