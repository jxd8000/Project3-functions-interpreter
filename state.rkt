#lang racket

;;;; ***************************************************
;;;; Jianhao Deng
;;;; Darion Gomez
;;;; Chloe de Lamare
;;;; CSDS 345 Spring 2026
;;;; Project 1
;;;; ***************************************************

(provide
 empty-state
 state-has?
 state-lookup
 state-declare
 state-declare/init
 state-update
 state-set-return
 state-get-return)

; create an empty state
; _ -> state
(define empty-state
  (lambda ()
    '()))

; check whether variable has been declared in state
; symbol state -> boolean
(define state-has?
  (lambda (var st)
    (cond
      ((null? st) #f)
      ((eq? var (first (first st))) #t)
      (else (state-has? var (rest st))))))

; retrieve the value bound to variable in state
; symbol state -> value
(define state-lookup
  (lambda (var st)
    (cond
      ((null? st) (error (format "Undeclared variable: ~a" var)))
      ((eq? var (first (first st))) (second (first st)))
      (else (state-lookup var (rest st))))))

; declare a new variable in the state
; symbol state -> state
(define state-declare
  (lambda (var st)
    (if (state-has? var st) (error (format "Variable already declared: ~a" var))
        (cons (list var 'UNINITIALIZED) st))))

; declare a new variable and initialize it with a value
; symbol value state -> state
(define state-declare/init
  (lambda (var val st)
    (if (state-has? var st)
        (error (format "Variable already declared: ~a" var))
        (cons (list var val) st))))

; update a existing variable's value in state
; symbol value state -> state
(define state-update
  (lambda (var val st)
    (cond
      ((null? st) (error (format "Undeclared variable: ~a" var)))
      ((eq? var (first (first st))) (cons (list var val) (rest st)))
      (else (cons (first st) (state-update var val (rest st)))))))
    
; store the program's return value into the state under the special name' return
; value state -> state
(define state-set-return
  (lambda (val st)
    (if (state-has? 'return st)
        (state-update 'return val st)
        (state-declare/init 'return val st))))

; get the program's return value from the state
; state -> value
(define state-get-return
  (lambda (st)
    (state-lookup 'return st)))
