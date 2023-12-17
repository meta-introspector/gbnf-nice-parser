(lang dune 3.12)
(using menhir 2.0)
(generate_opam_files true)
(name gbnf_nice_parser)
(version 1.0.0)
(license GPL)
(source
 (github meta-introspector/nice-parser))
(authors "Mike Dupont <jmikedupont2@gmail.com>")
(package
 (name gbnf_nice_parser)
 (synopsis "Using Nice parsers without the boilerplate to parse gbnf")
 (description
  "GBNF_Nice_parser wraps your {menhir, ocamlyacc}-generated parser in a sane interface, eliminating boilerplate code.")
 (depends
  (ocaml
   (>= 4.08.0))
  (dune
   (and
    :build
    (>= 1.10)))
  (stdio
   (<> 0))
  (base :with-test)
  (menhir :with-test)
  (ppx_jane :with-test)
  (odoc :with-doc)))