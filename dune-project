(lang dune 3.11)

(name compose_news)

(generate_opam_files true)

(package
 (allow_empty)
 (name compose_news)
 (depends
  ocaml
  dune
  angstrom
  js_of_ocaml
  ppx_deriving_yojson
  promise_jsoo
  re
  xml-light))
