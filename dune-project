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
 (depends
   (ocaml (>= "4.10"))
   unix
   dune
   fmt
   angstrom
   (containers (>= "3.6"))
   (timedesc (>= "0.9.1"))
   )
 (tags
  (
   "strace"
   "process monitoring"
   ))
 )

(package
 (name stramon)
 (synopsis "strace based process behavior monitoring library")
 (description "")
 (depends
   ocaml
   dune
   (stramon-lib (= :version))
   )
 (tags
  ("strace"
   "process monitoring"
   ))
 )

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
