SRCFILES = bin/*.ml lib/*.ml test/*.ml

OCPINDENT = ocp-indent \
	--inplace \
	$(SRCFILES)

.PHONY: all
all:
	dune build @all

.PHONY: release-static
release-static :
	OCAMLPARAM='_,ccopt=-static' dune build --release bin/stramon.exe

.PHONY: run
run:
	dune exec -- bin/stramon.exe -f -o abcd/defg/test.json ls

.PHONY: format
format :
	$(OCPINDENT)

.PHONY : clean
clean:
	dune clean
