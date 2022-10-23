(lang dune 3.4)

(name stramon)

(generate_opam_files true)

(source
 (github darrenldl/stramon))

(authors "Stramon developers")

(maintainers "Darren Ldl")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name stramon-lib)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

(package
 (name stramon)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune stramon-lib)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
