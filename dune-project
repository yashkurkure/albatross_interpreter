(lang dune 3.1)

(name albatross_interpreter)

(using menhir 2.1)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name albatross_interpreter)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune menhir sedlex)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
