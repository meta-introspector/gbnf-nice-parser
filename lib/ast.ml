open Sexplib.Std

type sexp =
  | Atom of string
  | List of sexp list
  [@@deriving sexp]
