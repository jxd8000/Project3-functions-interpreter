# Project 1: Simple Language Interpreter (Racket)

## Folder layout
- simpleParser.rkt / lex.rkt: professor provided parser + lexer
- sample_programs/: input programs for parser/interpreter
- tests/: racket test files
- state.rkt: state abstraction layer (A)
- eval.rkt: expression evaluation (B)
- interp.rkt: statement execution + interpret entry (C)

## Quick start (Parser smoke test)
1. Open DrRacket
2. Open `smoke.rkt` (or create it using the code below)
3. Run

### smoke.rkt
```racket
#lang racket
(require "simpleParser.rkt")
(define ast (parser "sample_programs/t01.txt"))
(displayln ast)

## Return handling rule (important)
- When executing `(return expr)`, interpreter must compute expr, call `state-set-return`, and stop executing remaining statements.
- When executing a statement list, after each statement, if `(state-has? 'return st)` is true, stop and return the current state.
