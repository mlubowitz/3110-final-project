.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/test.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/test.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

clean: bisect-clean
	dune clean
	rm -f search.zip

doc:
	dune build @doc

launch:
	OCAMLRUNPARAM=b dune exec src/gui.exe

zip:
	zip -r chess.zip . -x _build/\* .git/\*