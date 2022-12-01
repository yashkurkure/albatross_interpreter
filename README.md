Albatross Interpreter

An interpreter for the ***albatross programming language*** developed in CS473: Compiler Design - Fall 22 and CS476: Programming Language Design - Fall 22 at UIC.

The interpreter is written using OCaml that uses sedlex (Lexer) and Menhir (LR(1) parser) opam packages.

The interpreter works in stages:

- Lexing: extracting tokens
  
- Parsing: checking program syntax
  
  - AST generation
    
- Semantic analysis
  
  - Symbol resolution
    
  - Type checking
    
- Program evaluation: executes the program
  

## Usage

Use the `albatrossin.exe` binary to execute programs.

```bash
./albatross.exe <program.albatross>
```

Try executing the programs in `./examples`.

```bash
# factorial.albatross prints the factorials of 1-15
./albatross.exe ./examples/factorial.albatross


# pattern.albatross prints a * pattern
./albatrossin.exe ./examples/pattern.albatross
```

## Build Executable 🚧

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
