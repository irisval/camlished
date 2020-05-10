(lang dune 2.0)
(generate_opam_files true)

(source (github irisval/camlished))
(license MIT)
(authors "Bryan Tabor <bjt84@cornell.edu>"
    "Changyuan Lin <cl859@cornell.edu>"
    "Iris Kim <iyk7@cornell.edu>")
(maintainers "")

(package
 (name camlished)
 (synopsis "Banished-inspired city-builder in OCaml")
 (description "Banished-inspired city-builder in OCaml")
 (depends (dune (> 1.5))))