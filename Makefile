SRCFILES = src/*.ml src/*.mli

OCPINDENT = ocp-indent \
	--inplace \
	$(SRCFILES)

.PHONY: all
all:
	dune build @all

.PHONY: release-static
release-static :
	OCAMLPARAM='_,ccopt=-static' dune build --release src/stramon.exe

.PHONY: format
format :
	$(OCPINDENT)

.PHONY : clean
clean:
	dune clean
