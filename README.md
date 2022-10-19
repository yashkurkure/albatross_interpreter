# Albatross Interpreter

This is an interpreter written in Ocaml using sedlex and menhir. The project uses dune as the build system.

# IN PROGRESS ⚠️
This is a work in progress.
As of right now this interpreter has a partially working lexer and parser.
Once the parser is complete, the next stage would be to start building an AST that can be used to perform semantic analysis.

## Build Project 🚧
```
make
//or
dune build @all
```

## Clean Project 🧹
```
make clean
//or
dune clean
```

## Run Parser 🏃

```
dune exec albatross_interpreter
//or
make run
```
Then type the program on the command line, hit enter and press CTRL+D.

## Run Parser (Input in file) 🏃 + 📄
You can also pass it a program as a file:
```
dune exec albatross_interpreter < program.albatross
```

## Testing

Use the `runtests.sh` script for testing.

```
./runtests.sh
```
Look into the `/tests` directory to view the tests files and the expected outputs.

You can run tests individually like:

```
// run test 01
./runtests "01"

// run tests 01 and 04
./runtests "01|04"
```
