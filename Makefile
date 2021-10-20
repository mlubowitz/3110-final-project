# THIS FILE NEEDS TO BE EDITED AND COMPLETED
# DON'T KNOW IF THIS STUFF IS CORRECT


.PHONY: test check

build: dune build

utop: OCAMLRUNPARAM=b dune utop src

test: OCAMLRUNPARAM=b dune exec test/test.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

clean: dune clean

doc: dune build @doc

launch: OCAMLRUNPARAM=b dune exec src/gui.exe

zip: 
  zip -r chess.zip . -x@exclude.lst