FROM docker.io/ocaml/opam:alpine-ocaml-4.14
USER root
RUN apk add strace
RUN opam init --disable-sandboxing
RUN opam install dune containers fmt
RUN opam install utop ocp-indent
RUN opam install ansiterminal
RUN opam install oseq
RUN opam install angstrom
RUN opam install alcotest qcheck qcheck-alcotest
RUN opam install timedesc
RUN opam install yojson
RUN apk add libexecinfo-static
RUN opam install odoc
