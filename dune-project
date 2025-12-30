(lang dune 3.20)

(name aoc_2025)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name aoc_2025)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  base
  stdio
  ppx_assert
  ppx_compare
  ppx_inline_test
  ppx_sexp_conv
  (ocamlformat :dev)
  (ounit2 :with-test))
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
