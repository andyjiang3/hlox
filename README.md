# hlox

A tree-walking interpreter for Lox written in Haskell.

Lox is a dynamically typed language with a mix of Java and C features, Javascript-like syntax, and precedence that closely matches C. This was designed for [Crafting Interpreters](https://craftinginterpreters.com/), an educational compilers book.

We implemented an extended subset of the language. Our Lox interpreter supports:
* Basics (functional Lox program)
* Comments
* Lexical scoping
* Anonymous functions
* First-class functions
* Lexical closure
* Arrays
* Runtime errors
* Type-directed property testing with QuickCheck

## Building, running, and testing

#### Building
This project compiles with `stack build`. 

#### Running 
You can run the main executable with `stack run`. This starts a CLI that steps through a Lox program.
- `:l (program path)` to load a Lox program into the stepper
- `:n (number of statements to step forward)` to step forward x statements
- `:p (number of statements to step backwards)` to step backwards x statements
- `:r` to run current block to completion
- `:d` to dump the store

#### Testing
You can run the tests with `stack test`. Finally, you can start a REPL with `stack ghci`.

## Module organization

The source code into three separate places:

  - The bulk of our code is a reusable library in 
    modules in the `src` directory. 
    - [LoxSyntax.hs](src/LoxSyntax.hs) provides the types for the Lox syntax as well as pretty printing. 
    - The main parsing logic is implemented under [LoxParser.hs](src/LoxParser.hs) that uses [ParserLib.hs](src/ParserLib.hs), a applicative-based parsing library. 
    - [LoxStepper.hs](src/LoxStepper.hs) contains our evaluation that takes in an AST of a lox program intepetered by the parser. It uses the [State.hs](src/State.hs), a simplified generic version of state-transformer, for state management
    - [LoxArbitrary.hs](src/LoxArbitrary.hs) generates random values of types. It is  used to generate a random Lox program used for property-based testing.

  - The entry point for our executable is in [Main.hs](app/Main.hs). We provide a CLI tool that steps through a Lox program.
  
  - All of our test cases is in [the test directory](test/Spec.hs). Our testing is broken down into parser testing ([ParserTests.hs](test/ParserTests.hs)) and evaluation testing ([StepperTests.hs](test/StepperTests.hs)). Parser testing includes unit testing for helper functions and individual parsers. Additionally, we wrote round-tripping properties that:
    1. Takes a random value, expression, or statement of Lox, 
    2. Pretty print and parse it
    3. Match it back with the provided input

## Language reference

The grammar can be described as follows:

```
Block ::= {Statement} 

Statement 
  ::= {LValue} = {Expression}
  | var {Name} = {Expression} 
  | if ({Expression}) { {Block} } else { {Block} }
  | while ({Expression}) { {Block} }
  | for (var <LValue> = {Expression}; {Expression}; {Expression}) { {Block} }
  | f({Expression}, ..., {Expression})
  | fun {Name}({Name}, ..., {Name}) { {Block} }
  | return {Expression}
  | ‘;’

Expression
  ::= {Name}
  | {Value}
  | {Uop} {Expression}
  | {Expression} {Bop} {Expression}
  | {Expression}[{Expression}]
  | [{Expression}, ..., {Expression}]
  | f({Expression}, ..., {Expression}) 

Name ::= LiteralString

LValue
  ::= {Name}
  | {LValue}[{Expression}]

Value 
  ::= nil
  | false
  | true
  | LiteralString
  | Numeral
  | [{Value}, ..., {Value}]
  | \({Name}, ..., {Name})

Uop
  ::= ‘-’
  | ‘!’

Bop
  ::= ‘+’
  | ‘-’
  | ‘*’
  | ‘/’
  | ‘%’
  | ‘or’
  | ‘and’
  | ‘==’
  | ‘!=’
  | ‘>’
  | ‘>=’
  | ‘<’
  | ‘<=’
```

## Importing additional libraries

This project is designed to run with stackage: you can easily use any library in https://www.stackage.org/lts-21.6 by adding an entry to the `build-depends` list of the `common-stanza` in the [cabal file](project-cis5520.cabal). If you want to use a library that is not on stackage, you'll need to update the common-stanza *and* add information to `stack.yaml` about where to find that library.

## Authors

Andy Jiang (jianga), Mohamed Alnasir (alnasir7), Tinatin Kokoshvili (tinatin)
