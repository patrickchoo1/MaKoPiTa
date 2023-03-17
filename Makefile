.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

test:
	OCAMLRUNPARAM=b dune exec src/main.exe

check:
	@bash check.sh
