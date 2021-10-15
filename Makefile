# THIS FILE NEEDS TO BE EDITED AND COMPLETED
# DON'T KNOW IF THIS STUFF IS CORRECT


.PHONY: test check

build: dune build

utop: OCAMLRUNPARAM=b dune utop src

test: OCAMLRUNPARAM=b dune exec test/test.exe

clean: dune clean

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

doc: dune build @doc

launch: OCAMLRUNPARAM=b dune exec src/gui.exe