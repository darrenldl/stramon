SRCFILES = bin/*.ml lib/*.ml test/*.ml

OCPINDENT = ocp-indent \
	--inplace \
	$(SRCFILES)

.PHONY: all
all:
	dune build @all

.PHONY: release-static
release-static :
	OCAMLPARAM='_,ccopt=-static' dune build --release src/stramon.exe

.PHONY: run
run:
	dune exec bin/stramon.exe

.PHONY: format
format :
	$(OCPINDENT)

.PHONY : clean
clean:
	dune clean
