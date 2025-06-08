# calculator

> A statically typed calculator language with type checking and compilation features

This project is a calculator language implementation written in Haskell, focusing on type safety and compilation theory. It's designed as a learning exercise to explore language design and compilation techniques.

## Features

- **Rich Type System**
  - Integers and floating-point numbers
  - Boolean values
  - Strings
  - Tuples
  - Unit type

- **Expressions and Operations**
  - Arithmetic operations (+, -, *, /)
  - Comparison operations (==, !=)
  - Logical operations (&&, ||, !)
  - Parenthesized expressions

- **Control Flow**
  - If-else statements
  - While loops
  - Code blocks
  - Expression sequences

- **Variables and Functions**
  - Variable declarations with let bindings
  - Function calls
  - Type inference and checking

## Technical Details

The implementation includes:
- Concrete Syntax Tree (CST) parsing
- Abstract Syntax Tree (AST) generation
- Type checking and inference
- Desugaring phase
- Compilation pipeline

## Testing

You can find all test cases under `test/Spec.hs`

