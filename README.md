# Albatross Interpreter

An interpreter for the ***albatross programming language*** developed in CS473: Compiler Design - Fall 22 and CS476: Programming Language Design - Fall 22 at UIC.

The interpreter is written using OCaml that uses sedlex (Lexer) and Menhir (LR(1) parser) opam packages.

The interpreter works in 4 stages:

1. Lexing: extracting tokens
  
2. Parsing: checking program syntax
  
  - AST generation
    
3. Semantic analysis
  
  - Symbol resolution
    
  - Type checking
    
4. Program evaluation: executes the program
  

## Usage

Use the `albatrossin.exe` binary to execute programs.

```bash
./albatross.exe <program.albatross>
```

Try executing the programs in `./examples`.

```bash
# factorial.albatross prints the factorials of 1-15
./albatrossin.exe ./examples/factorial.albatross


# pattern.albatross prints a * pattern
./albatrossin.exe ./examples/pattern.albatross
```
Note that the dune build system adds the `.exe` extension to the executable to maintain consistency accross Windows, Linux and MacOS. The extension `.exe` is meaningless to Linux and MacOS, but Windows requires it for executables.

## Build Executable 🏗️

###### Prerequisites

Ocaml/Opam: [Get Up and Running With OCaml · OCaml Tutorials](https://ocaml.org/docs/up-and-running)

Dune Build: [Install Dune](https://dune.build/install)

###### Build

Clone the project and cd into the project root:

```bash
clone git@github.com:yashkurkure/albatross_interpreter.git
cd ./albatross_interpreter
```

Build using Opam:

```bash
# Install project dependencies
opam install . --deps-only --with-test

# Build the project
opam exec -- dune build
```

Alternaitvely, build using Dune:

```bash
# Install project dependencies and build project
dune build
```

Building the project will create `albatrossin.exe` in the root of the project.

## Executing using Dune 🏃

You can build and execute the project at once using dune.

```bash
dune exec albatrossin <program.albatross>
```
## Testing

You can run test scipts for testing the parser, semantic analysis and execution stages.

The branches of this repository `lexer_and_parser`, `semantic_analysis` and `master` are symbolic to the stages this interpreter was developed in (`master` branch has everything till the execution stage). 

Each branch has a `runtests.sh` script which executes the programs in the `./tests` and compares the output with the expected output.

This repository also has github actions configured to run on `push` the `runtests.sh` script and check if all tests pass.

