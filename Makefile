.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

<<<<<<< HEAD
utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec src/main.exe

clean:
	dune clean

=======
test:
	OCAMLRUNPARAM=b dune exec src/main.exe

check:
	@bash check.sh
>>>>>>> b68b8d5972eb865d32c45e2f5dda222105f85a54
