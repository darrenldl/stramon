SRCFILES = bin/*.ml lib/*.ml lib/*.mli lib-tests/*.ml

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
	dune exec -- bin/stramon.exe -f -o test.json ls

.PHONY: lib-tests
lib-tests :
	OCAMLRUNPARAM=b dune exec lib-tests/main.exe --no-buffer --force

.PHONY: format
format :
	$(OCPINDENT)

.PHONY : clean
clean:
	dune clean
