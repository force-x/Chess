build:
	dune build

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

test:
	OCAMLRUNPARAM=b dune exec tests/main.exe

utop:
	OCAMLRUNPARAM=b dune utop src

doc:
	dune build @doc
