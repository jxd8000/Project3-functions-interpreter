#lang racket
(require "simpleParser.rkt")

(define ast (parser "sample_programs/t01.txt"))
(displayln ast)
