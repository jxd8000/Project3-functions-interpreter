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
 push-layer
 pop-layer
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
    (list '())))

; add a new empty top layer
; state -> state
(define push-layer
  (lambda (st)
    (cons '() st)))

; remove top layer
; state -> state
(define pop-layer
  (lambda (st)
    (if (null? st)
        (error "Cannot pop from empty state")
        (rest st))))

; check whether variable has been declared in state
; symbol state -> boolean
(define state-has?
  (lambda (var st)
    (cond
      ((null? st) #f)
      (else
       (or (layer-has? var (first st))
           (state-has? var (rest st)))))))

(define layer-has?
  (lambda (var layer)
    (cond
      ((null? layer) #f)
      ((eq? var (first (first layer))) #t)
      (else (layer-has? var (rest layer))))))

; retrieve the value bound to variable in state
; symbol state -> value
(define state-lookup
  (lambda (var st)
    (cond
      ((null? st) (error (format "Undeclared variable: ~a" var)))
      
      (else (layer-lookup var (first st) (rest st))))))

(define layer-lookup
  (lambda (var layer rest-state)
    (cond
      ((null? layer) (state-lookup var rest-state))
      ((eq? var (first (first layer))) (second (first layer)))
      (else (layer-lookup var (rest layer) rest-state)))))

; declare a new variable in the state
; symbol state -> state
(define state-declare
  (lambda (var st)
    (if (state-has? var st) (error (format "Variable already declared: ~a" var))
        (cons (cons (list var 'UNINITIALIZED) (first st)) (rest st)))))

; declare a new variable and initialize it with a value
; symbol value state -> state
(define state-declare/init
  (lambda (var val st)
    (if (state-has? var st)
        (error (format "Variable already declared: ~a" var))
        (cons (cons (list var val) (first st)) (rest st)))))

; update a existing variable's value in state
; symbol value state -> state
(define state-update
  (lambda (var val st)
    (cond
      ((null? st) (error (format "Undeclared variable: ~a" var)))

      (else
       (let ((updated-top (layer-update var val (first st))))
         (if updated-top
             (cons updated-top (rest st))
             (cons (first st) (state-update var val (rest st)))))))))

(define layer-update
  (lambda (var val layer)
    (cond
      ((null? layer) #f)
      ((eq? var (first (first layer)))
       (cons (list var val) (rest layer)))
      (else
       (let ((updated-rest (layer-update var val (rest layer))))
         (if updated-rest (cons (first layer) updated-rest) #f))))))
    
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
























