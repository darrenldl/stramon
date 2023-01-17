(lang dune 3.4)

(name stramon)

(generate_opam_files true)

(source
 (github darrenldl/stramon))

(authors "Darren Ldl")

(maintainers "Darren Ldl")

(license MIT)

(package
 (name stramon-lib)
 (synopsis "Process behavior monitoring library based on strace")
 (description "
Stramon-lib is primarily designed to power Stramon,
or to be used in similar fashion of process monitoring.
")
 (documentation https://ocaml.org/p/stramon-lib)
 (depends
   (ocaml (>= "4.08"))
   base-unix
   dune
   seq
   (fmt (>= "0.9.0"))
   (angstrom (>= "0.15.0"))
   (containers (>= "3.6"))
   (alcotest :with-test)
   (qcheck-alcotest :with-test)
   (qcheck :with-test)
   )
 (tags
  (
   "strace"
   "process monitoring"
   ))
 )

(package
 (name stramon)
 (synopsis "Process behavior monitoring utility based on strace")
 (description "")
 (documentation https://github.com/darrenldl/stramon)
 (depends
   ocaml
   dune
   (stramon-lib (>= "0.2.0"))
   (timedesc (>= "0.9.1"))
   )
 (tags
  ("strace"
   "process monitoring"
   ))
 )

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
