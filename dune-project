(lang dune 3.4)

(name stramon)

(generate_opam_files true)

(source
 (github darrenldl/stramon))

(authors "Darren Ldl")

(maintainers "Darren Ldl")

(license MIT)

(documentation https://github.com/darrenldl/stramon)

(package
 (name stramon-lib)
 (synopsis "strace based process behavior monitoring library")
 (description "")
 (depends ocaml dune fmt angstrom unix containers)
 (tags
  (topics "to describe" your project)))

(package
 (name stramon)
 (synopsis "strace based process behavior monitoring library")
 (description "")
 (depends ocaml dune stramon-lib)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
