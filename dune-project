(lang dune 3.4)

(name athena-tools)

(generate_opam_files true)

(source
 (github d0p1s4m4/athena-tools))

(authors "d0p1")
(maintainers "d0p1")

(license "BSD-3-Clause")

(using menhir 2.0)

(package
 (name athena-tools)
 (synopsis "Athena Assembler and Disassembler")
 (description "Athena Assembler and Disassembler")
 (depends
  ocaml
  dune
  cmdliner
  menhir
  (alcotest :with-test)
  (bisect_ppx :with-test))
 (tags
  (topics assembler)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
