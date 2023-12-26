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

{

open Lexing
open SentenceParser
open Positions
open Keyword

(* ------------------------------------------------------------------------ *)

(* Short-hands. *)

let error1 pos =
  Error.error (Positions.one pos)

let error2 lexbuf =
  Error.error (Positions.lexbuf lexbuf)

(* ------------------------------------------------------------------------ *)

(* [int_of_string] raises [Failure] if its argument is too large. This is
   not a problem in practice, but causes false positives when fuzzing
   Menhir. We hide the problem by failing gracefully. *)

let int_of_string (pos : Lexing.position) i =
  try
    int_of_string i
  with Failure _ ->
    error1 pos "unreasonably large integer."

(* ------------------------------------------------------------------------ *)

(* This wrapper saves the current lexeme start, invokes its argument,
   and restores it. This allows transmitting better positions to the
   parser. *)

let savestart lexbuf f =
  let startp = lexbuf.lex_start_p in
  let token = f lexbuf in
  lexbuf.lex_start_p <- startp;
  token

(* ------------------------------------------------------------------------ *)

(* Overwrites an old character with a new one at a specified
   offset in a [bytes] buffer. *)

let overwrite content offset c1 c2 =
  assert (Bytes.get content offset = c1);
  Bytes.set content offset c2

(* ------------------------------------------------------------------------ *)

(* Keyword recognition and construction. *)

(* A monster is a spot where we have identified a keyword in concrete syntax.
   We describe a monster as an object with the following methods: *)

type monster = {

  (* The position of the monster. *)
  pos: Positions.t;

  (* This method is passed an array of (optional) names for the producers,
     that is, the elements of the production's right-hand side. It is also
     passed a flag which tells whether [$i] syntax is allowed or disallowed.
     It may perform some checks and is allowed to fail. *)
  check: check;

  (* This method transforms the keyword (in place) into a conventional
     OCaml identifier. This is done by replacing '$', '(', and ')' with
     '_'. Bloody. The arguments are [ofs1] and [content]. [ofs1] is the
     offset where [content] begins in the source file. *)
  transform: int -> bytes -> unit;

  (* This is the keyword, in abstract syntax. *)
  keyword: keyword option;

  (* If this is a [$i] monster, then the identifier [_i] is stored here. *)
  oid: string option;

}

and check =
  string option array -> unit

(* No check. *)

let none : check =
  fun _ -> ()

(* ------------------------------------------------------------------------ *)

(* The [$syntaxerror] monster. *)

let syntaxerror pos : monster =
  let check =
    none
  and transform ofs1 content =
    (* [$syntaxerror] is replaced with
       [(raise _eRR)]. Same length. *)
    let pos = start_of_position pos in
    let ofs = pos.pos_cnum - ofs1 in
    let source = "(raise _eRR)" in
    Bytes.blit_string source 0 content ofs (String.length source)
  and keyword =
    Some SyntaxError
  and oid =
    None
  in
  { pos; check; transform; keyword; oid }

(* ------------------------------------------------------------------------ *)

(* We check that every [$i] is within range. Also, we forbid using [$i]
   when a producer has been given a name; this is bad style and may be
   a mistake. (Plus, this simplifies our life, as we rewrite [$i] to [_i],
   and we would have to rewrite it to a different identifier otherwise.) *)

(* let check_dollar pos i : check = fun dollars producers -> *)
(*   (\* If [i] is out of range, say so. *\) *)
(*   if not (0 <= i - 1 && i - 1 < Array.length producers) then *)
(*     Error.error [pos] "$%d refers to a nonexistent symbol." i; *)
(*   (\* If [$i] could be referred to via a name, say so. *\) *)
(*   producers.(i - 1) |> Option.iter (fun x -> *)
(*     Error.error [pos] "please do not say: $%d. Instead, say: %s." i x *)
(*   ); *)
(*   (\* If [$i] syntax is disallowed, say so. *\) *)
(*   match dollars with *)
(*   | Settings.DollarsDisallowed -> *)
(*       Error.error [pos] "please do not use $%d. Instead, name this value." i *)
(*   | Settings.DollarsAllowed -> *)
(*       () *)

(* We check that every reference to a producer [x] in a position keyword,
   such as [$startpos(x)], exists. *)

(* let check_producer pos x : check = fun _ producers -> *)
(*   if not (List.mem (Some x) (Array.to_list producers)) then *)
(*     Error.error [pos] "%s refers to a nonexistent symbol." x *)

(* ------------------------------------------------------------------------ *)

(* The [$i] monster. *)

(* let dollar pos i : monster = *)
(*   let check : check = (\* check_dollar pos i *\\) *\) *)
(*    transform ofs1 content = *)
(*     (\* [$i] is replaced with [_i]. Thus, it is no longer a keyword. *\) *)
(*     let pos = start_of_position pos in *)
(*     let ofs = pos.pos_cnum - ofs1 in *)
(*     overwrite content ofs '$' '_' *)
(*   and keyword = *)
(*     None *)
(*   and oid = *)
(*     Some (Printf.sprintf "_%d" i) *)
(*   in *)
(*   { pos; check; transform; keyword; oid } *)

(* ------------------------------------------------------------------------ *)

(* The position-keyword monster. The most horrible of all. *)

let position pos
  (where : string)
  (flavor : string)
  (i : string option) (x : string option)
=
  let check_no_parameter () =
    if i <> None || x <> None then
      Error.error [pos] "$%s%s does not take a parameter." where flavor
  in
  let ofslpar = (* offset of the opening parenthesis, if there is one *)
    1 + (* for the initial "$" *)
    String.length where +
    3   (* for "pos" or "ofs" or "loc" *)
  in
  let where =
    match where with
    | "symbolstart"
    | "s"           -> check_no_parameter(); WhereSymbolStart
    | "start"       -> WhereStart
    | "end"         -> WhereEnd
    | ""            -> WhereStart
    | _             -> assert false
  in
  let flavor =
    match flavor with
    | "pos"   -> FlavorPosition
    | "ofs"   -> FlavorOffset
    | "loc"   -> FlavorLocation
    | _       -> assert false
  in
  let subject, check =
    match i, x with
    (* | Some i, None -> *)
    (*     let ii = int_of_string (start_of_position pos) i in *)
    (*     if ii = 0 && where = WhereEnd then *)
    (*       (\* [$endpos($0)] *\) *)
    (*       Before, none *)
    (*     (\* else *\) *)
    (*     (\*   (\\* [$startpos($i)] is rewritten to [$startpos(_i)]. *\\) *\) *)
    (*     (\*   RightNamed ("_" ^ i), check_dollar pos ii *\) *)
    (* | None, Some x -> *)
    (*     (\* [$startpos(x)] *\) *)
    (*     RightNamed x, check_producer pos x *)
    | None, None ->
        (* [$startpos] *)
        Left, none
    | Some _, Some _ ->
        assert false
  in
  let transform ofs1 content =
    let pos = start_of_position pos in
    let ofs = pos.pos_cnum - ofs1 in
    overwrite content ofs '$' '_';
    let ofslpar = ofs + ofslpar in
    match i, x with
    | None, Some x ->
        overwrite content ofslpar '(' '_';
        overwrite content (ofslpar + 1 + String.length x) ')' '_'
    | Some i, None ->
        overwrite content ofslpar '(' '_';
        overwrite content (ofslpar + 1) '$' '_';
        overwrite content (ofslpar + 2 + String.length i) ')' '_'
    | _, _ ->
        ()
  in
  let keyword =
    Some (Position (subject, where, flavor))
  and oid =
    None
  in
  { pos; check; transform; keyword; oid }

(* ------------------------------------------------------------------------ *)

(* In an OCaml header, there should be no monsters. This is just a sanity
   check. *)

let no_monsters monsters =
  match monsters with
  | [] ->
      ()
  | monster :: _ ->
      Error.error [monster.pos]
        "a Menhir keyword cannot be used in an OCaml header."

(* ------------------------------------------------------------------------ *)

(* Gathering all of the identifiers in an array of optional identifiers. *)

let gather_oid xs oid =
  match oid with
  | Some x ->
      StringSet.add x xs
  | None ->
      xs

let gather_oids oids =
  Array.fold_left gather_oid StringSet.empty oids

(* Gathering all of the [oid] identifiers in a list of monsters. *)

let gather_monsters monsters =
  List.fold_left (fun xs monster ->
    gather_oid xs monster.oid
  ) StringSet.empty monsters

(* ------------------------------------------------------------------------ *)

(* Creates a stretch. *)

let mk_stretch pos1 pos2 parenthesize monsters =
  (* Read the specified chunk of the file. *)
  let raw_content : string = InputFile.chunk (pos1, pos2) in
  (* Transform the monsters, if there are any. (This explicit test
     allows saving one string copy and keeping just one live copy.) *)
  let content : string =
    match monsters with
    | [] ->
        raw_content
    | _ :: _ ->
        let content : bytes = Bytes.of_string raw_content in
        List.iter (fun monster -> monster.transform pos1.pos_cnum content) monsters;
        Bytes.unsafe_to_string content
  in
  (* Add whitespace so that the column numbers match those of the source file.
     If requested, add parentheses so that the semantic action can be inserted
     into other code without ambiguity. *)
  let content =
    if parenthesize then
      (* If [parenthesize] is true then we are at the beginning of a semantic
         action, just after the opening brace. This guarantees that we cannot
         be at the beginning of a line, so the subtraction [_ - 1] below
         cannot produce a negative result. *)
      (String.make (pos1.pos_cnum - pos1.pos_bol - 1) ' ') ^ "(" ^ content ^ ")"
    else
      (String.make (pos1.pos_cnum - pos1.pos_bol) ' ') ^ content
  in
  Stretch.({
    stretch_filename = InputFile.get_input_file_name();
    stretch_linenum = pos1.pos_lnum;
    stretch_linecount = pos2.pos_lnum - pos1.pos_lnum;
    stretch_content = content;
    stretch_raw_content = raw_content;
    stretch_keywords = Misc.filter_map (fun monster -> monster.keyword) monsters
  })

(* Creating a stretch from a located identifier. (This does not require the
   input file to be currently opened.) In this variant, [parenthesize] is
   false, [monsters] is empty. *)

let stretch_of_id (id : string located) =
  let raw_content, pos = Positions.decompose id in
  let pos1 = Positions.start_of_position pos
  and pos2 = Positions.end_of_position pos
  and filename = Positions.filename_of_position pos in
  assert (pos1 != Lexing.dummy_pos);
  let padding = pos1.pos_cnum - pos1.pos_bol in
  let content = String.make padding ' ' ^ raw_content in
  Stretch.({
    stretch_filename = filename;
    stretch_linenum = pos1.pos_lnum;
    stretch_linecount = pos2.pos_lnum - pos1.pos_lnum;
    stretch_content = content;
    stretch_raw_content = raw_content;
    stretch_keywords = []
  })

(* ------------------------------------------------------------------------ *)

(* OCaml's reserved words. *)

let table words =
  let table = Hashtbl.create 149 in
  List.iter (fun word -> Hashtbl.add table word ()) words;
  table

let reserved =
  table [
    "and";
    "as";
    "assert";
    "begin";
    "class";
    "constraint";
    "do";
    "done";
    "downto";
    "else";
    "end";
    "exception";
    "external";
    "false";
    "for";
    "fun";
    "function";
    "functor";
    "if";
    "in";
    "include";
    "inherit";
    "initializer";
    "lazy";
    "let";
    "match";
    "method";
    "module";
    "mutable";
    "new";
    "object";
    "of";
    "open";
    "or";
    "parser";
    "private";
    "rec";
    "sig";
    "struct";
    "then";
    "to";
    "true";
    "try";
    "type";
    "val";
    "virtual";
    "when";
    "while";
    "with";
    "mod";
    "land";
    "lor";
    "lxor";
    "lsl";
    "lsr";
    "asr";
  ]

(* ------------------------------------------------------------------------ *)

(* Decoding escaped characters. *)

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

}

(* ------------------------------------------------------------------------ *)

(* Patterns. *)

let newline = ('\010' | '\013' | "\013\010")

let whitespace = [ ' ' '\t' ]

let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']

let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']

let identchar = ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '0'-'9'] (* '\'' forbidden *)

let attributechar = identchar | '.'

let subject =
  '$' (['0'-'9']+ as i)
| ((lowercase identchar*) as x)

let poskeyword =
  '$'
  (
    (("symbolstart" | "start" | "end") as where) (("pos" | "ofs") as flavor)
  | (("s" | "") as where) ("loc" as flavor)
  )
  ( '(' subject ')' )?

let previouserror =
  "$previouserror"

let syntaxerror =
  "$syntaxerror"

(* ------------------------------------------------------------------------ *)

(* The lexer. *)

rule main = parse

| ","
    { COMMA }

| "("
    {       (print_endline (Batteries.dump (((Lexing.lexeme lexbuf))))); LPAREN }

| ")"
    { RPAREN }
 | "[" 
    {
      (* lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;  *)
      (print_endline (Batteries.dump (((Lexing.lexeme lexbuf)))));
      let buffer = Buffer.create 256 in
      let openingpos = lexeme_start_p lexbuf in
      let content = charclass  openingpos buffer lexbuf in
      let id = Printf.sprintf "\"%s\"" content in
      let pos = import (openingpos, lexbuf.lex_curr_p) in
      REGEX (with_pos pos id)
    }


| "]"
    { RBRACE }
| "|"
    { BAR }
| "?"
    { QUESTION }
| "*"
    { STAR }
| "+"
    { PLUS }
| "-"
    { DASH }
| "^"
    { CARET }
| "::="
    {
      (print_endline "DEBUG");
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let tok = Lexing.lexeme lexbuf in
      (print_endline (Batteries.dump ((line,cnum,tok))));

      COLONCOLONEQUAL }

| ( identchar *) as id
    { 
      LID (with_pos (cpos lexbuf) id)
    }

(* Quoted strings are used as aliases for tokens. *)
(* A quoted string is stored as is -- with the quotes
   and with its escape sequences. *)
| '"'
    { let buffer = Buffer.create 16 in
      let openingpos = lexeme_start_p lexbuf in
      let content = record_string openingpos buffer lexbuf in
      let id = Printf.sprintf "\"%s\"" content in
      let pos = import (openingpos, lexbuf.lex_curr_p) in
      QID (with_pos pos id) }
| "//" [^ '\010' '\013']* newline (* skip C++ style comment *)
| newline 
     { NEWLINE }
| "#" [^'\010''\013']* newline { NEWLINE }
(*       (print_endline "NL1");	 *)
(*       (print_endline (Batteries.dump lexbuf));	 *)
(*       (print_endline "NL2");	 *)
(*       main lexbuf;  } *)
| whitespace+
    {
     (* (print_endline "WS1"); *)
     (*      (print_endline (Batteries.dump lexbuf)); *)
     (* (print_endline "WS2"); *)
      (*      main lexbuf ; *)
      (* WHITESPACE *)
      main lexbuf ; 
    }
| eof
    { EOF }

| _
    { error2 lexbuf "unexpected character(s)." }

and record_string openingpos buffer = parse
| '"'
    { Buffer.contents buffer }
| ('\\' ['\\' '\'' '"' 't' 'b' 'r' ' ']) as sequence
    { (* This escape sequence is recognized as such, but not decoded. *)
      Buffer.add_string buffer sequence;
      record_string openingpos buffer lexbuf }
| newline
    { error2 lexbuf "illegal newline in string." }
| eof
    { error1 openingpos "unterminated string." }
| _ as c
    { Buffer.add_char buffer c;
      record_string openingpos buffer lexbuf }

and charclass openingpos buffer = parse
| "]" { (print_endline (Batteries.dump (((Lexing.lexeme lexbuf)))));
        (* main lexbuf *)
        "TODO"
}
| "[" { (print_endline (Batteries.dump (((Lexing.lexeme lexbuf)))));
        (* LBRACE *)
        
        charclass openingpos buffer lexbuf
}
| _ {
      (print_endline (Batteries.dump (((Lexing.lexeme lexbuf))))); 
        (* Tchar(Char.code(Lexing.lexeme_char lexbuf 0))  *)
        charclass openingpos buffer lexbuf
    }



