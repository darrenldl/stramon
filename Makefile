SRCFILES = gen/*.ml bin/*.ml lib/*.ml lib/*.mli lib-tests/*.ml

OCPINDENT = ocp-indent \
	--inplace \
	$(SRCFILES)

OPAMFILES = *.opam

PATCH_OPAMFILES = sed -i 's/"@runtest"\s*{with-test}//g' $(OPAMFILES)

.PHONY: all
all:
	./update-stramon-version-string.sh
	dune build @all

.PHONY: release-static
release-static :
	./update-stramon-version-string.sh
	OCAMLPARAM='_,ccopt=-static' dune build --release bin/stramon.exe
	mkdir -p statically-linked
	cp _build/default/bin/stramon.exe statically-linked/stramon

.PHONY: lib-tests
lib-tests :
	OCAMLRUNPARAM=b dune exec lib-tests/main.exe --no-buffer --force

.PHONY: doc
doc:
	dune build @doc

.PHONY: format
format :
	$(OCPINDENT)

.PHONY : clean
clean:
	dune clean
