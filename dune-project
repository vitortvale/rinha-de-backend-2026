(lang dune 3.11)
(name rinha_2026_ocaml)

(generate_opam_files true)

(package
 (name rinha_2026_ocaml)
 (depends
  (ocaml (>= 5.2))
  dune
  core
  eio
  eio_main))

(package
 (name rinha_2026_ocaml_lb)
 (depends
  (ocaml (>= 5.2))
  dune))
