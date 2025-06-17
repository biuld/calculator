# calculator

A statically typed calculator language written in Haskell with a focus on type safety, parsing, and compilation theory.

## Features

- **Rich Type System**
  - Integers and floating-point numbers
  - Boolean values
  - Strings
  - Tuples
  - Unit type
  - First-class functions

- **Expressions and Operations**
  - Arithmetic operations `(+ - * /)` for `Int` and `Double`
  - Comparison operations `(== !=)`
  - Logical operations `(&& || !)`
  - Parenthesised expressions

- **Control Flow**
  - `if … then … else …` expressions
  - `while` loops
  - Code blocks `{ e1; e2; … }`
  - Expression sequences

- **Variables and Functions**
  - `let` bindings (single and multiple)
  - Lambda expressions with explicit type annotations
  - Partial application & multi-argument functions
  - Hindley-Milner-style type inference during desugaring

## Getting Started

### Prerequisites

The easiest way to build the project is with [Stack](https://docs.haskellstack.org/). Any recent GHC (≥ 9.8) should work if you prefer Cabal.

### Build

```bash
# Clone and enter the repository
$ git clone https://github.com/biuld/calculator.git
$ cd calculator

# Build everything
$ stack build
```

### CLI Usage

The executable is exposed as `calculator-exe`. Use `stack run` (or Cabal's `cabal run`) to invoke it:

```bash
# General form
stack run calculator-exe -- <command> [-r] <expression>
```

*Commands*

- `parse`   – Parse the expression and pretty-print the Concrete Syntax Tree (CST)
- `desugar` – Parse and desugar the CST into the typed Abstract Syntax Tree (AST)
- `eval`    – Evaluate the expression with the small-step interpreter

Add the `-r` switch to display the raw `Show` instance instead of the pretty printer.

#### Examples

```bash
# Pretty print CST
stack run calculator-exe -- parse "(1 + 2) * 3"

# Show raw CST
stack run calculator-exe -- parse -r "(1 + 2) * 3"

# Desugar to AST
stack run calculator-exe -- desugar "let { f = \a:Int -> \b:Int -> a + b; x = 1; y = 2 } in f x y"

# Evaluate an expression
stack run calculator-exe -- eval "let {x = 10; y = 20} in x + y"
```

## Project Layout

```
src/
  Language/Calculator/
    CST/         -- Lexer, parser, tokens & CST pretty printer
    AST/         -- Typed AST, interpreter, value & environment utilities
    Desugar.hs   -- CST → AST desugaring with type checking
app/Main.hs      -- Command-line interface

test/            -- HSpec test-suite
```

## Running the Test-suite

```bash
stack test
```

The suite exercises both the parser and desugaring phases (see `test/ParserSpec.hs` and `test/DesugarSpec.hs`).

## Roadmap

Planned improvements include:

- Pattern matching
- Algebraic data types
- A proper optimiser
- Bytecode compilation and a VM back-end

## License

This project is licensed under the BSD-3-Clause license. See `LICENSE` for the full text.

