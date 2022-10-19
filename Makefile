.PHONY: all run clean
all:
	dune build @all

run:
	dune exec albatross

clean:
	dune clean