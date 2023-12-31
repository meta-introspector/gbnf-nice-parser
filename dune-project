(lang dune 3.5)

(name gbnf_parser)
(using menhir 2.0)
(generate_opam_files true)

(authors "James Michael DuPont")

(license GPLV3)

(source (github meta-introspector/gbnf-nice-parser))

(package
 (name gbnf_parser)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
