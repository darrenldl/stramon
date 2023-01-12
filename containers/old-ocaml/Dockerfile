FROM docker.io/ocaml/opam:ubuntu-20.04-ocaml-4.08
USER root
RUN apt-get --yes install strace
RUN opam init --disable-sandboxing
RUN opam install dune containers fmt
RUN opam install utop ocp-indent
RUN opam install ansiterminal
RUN opam install oseq
RUN apt-get --yes install pkg-config
RUN opam install angstrom
RUN opam install alcotest qcheck qcheck-alcotest
RUN opam install timedesc
RUN opam install yojson
RUN opam install odoc
