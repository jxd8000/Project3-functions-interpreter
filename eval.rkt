#lang racket

;;;; ***************************************************
;;;; Jianhao Deng
;;;; Darion Gomez
;;;; Chloe de Lamare
;;;; CSDS 345 Spring 2026
;;;; Project 2
;;;; ***************************************************


(require "state.rkt")

(provide
 M_expression
 M_boolean
 M_value)

;; Helper Functions -----------------------------------------------------------------------

;; abstraction functions
(define operator car)
(define operand1 cadr)
(define operand2 caddr)

;; checks if input is a boolean expression
(define condition?
  (lambda (e)
    (cond
      ((eq? e 'true)  #t)
      ((eq? e 'false) #f)
      (else (and (list? e)
                 (or (eq? (operator e) '&&)
                     (eq? (operator e) '||)
                     (eq? (operator e) '!)
                     (eq? (operator e) '<)
                     (eq? (operator e) '<=)
                     (eq? (operator e) '>)
                     (eq? (operator e) '>=)
                     (eq? (operator e) '==)
                     (eq? (operator e) '!=)))))))

;; checks if operator is binary
(define binary-opp?
  (lambda (e)
    (and (list? e) (= (length e) 3))))

;; checks if operator is unary
(define unary-opp?
  (lambda (e)
    (and (list? e) (= (length e) 2))))

;; returns an error if a variable is unassigned
(define lookup-variable
  (lambda (exp st)
    (let ([v (state-lookup exp st)])
      (if (eq? v 'UNINITIALIZED)
          (error 'badop "Unassigned variable: ~s" exp)
          v))))

;; ----------------------------------------------------------------------------------------

;; M_expression
;; expression state --> value
(define M_expression
  (lambda (expression st)
    (cond
      ((condition? expression) (M_boolean expression st))
      (else                    (M_value expression st)))))



;; M_cond
;; condition state --> boolean
(define M_boolean
  (lambda (expression st)
    (cond
      ((eq? expression 'true) #t)
      ((eq? expression 'false) #f)
      ((symbol? expression) (lookup-variable expression st))
      ((and (binary-opp? expression) (eq? '&& (operator expression)))
       (and (M_boolean (operand1 expression) st)
            (M_boolean (operand2 expression) st)))
      ((and (binary-opp? expression) (eq? '|| (operator expression)))
       (or (M_boolean (operand1 expression) st)
            (M_boolean (operand2 expression) st)))
      ((and (unary-opp? expression) (eq? '! (operator expression)))
       (not (M_boolean (operand1 expression) st)))
      ((and (binary-opp? expression) (eq? '< (operator expression)))
       (< (M_value (operand1 expression) st)
           (M_value (operand2 expression) st)))
      ((and (binary-opp? expression) (eq? '<= (operator expression)))
       (<= (M_value (operand1 expression) st)
           (M_value (operand2 expression) st)))
      ((and (binary-opp? expression) (eq? '> (operator expression)))
       (> (M_value (operand1 expression) st)
           (M_value (operand2 expression) st)))
      ((and (binary-opp? expression) (eq? '>= (operator expression)))
       (>= (M_value (operand1 expression) st)
           (M_value (operand2 expression) st)))
      ((and (binary-opp? expression) (eq? '== (operator expression)))
       (eq? (M_expression (operand1 expression) st)
            (M_expression (operand2 expression) st)))
      ((and (binary-opp? expression) (eq? '!= (operator expression)))
       (not (eq? (M_expression (operand1 expression) st)
                 (M_expression (operand2 expression) st))))
      (else (error 'M_boolean "Invalid boolean expression ~s" expression)))))
      
      


;; M-value
;; int-value state --> integer
(define M_value
  (lambda (expression st)
    (cond
      ((number? expression)
       expression)
      ((symbol? expression)
       (lookup-variable expression st))
      ((and (eq? '+ (operator expression)) (binary-opp? expression))
       (+         (M_value (operand1 expression) st)
                  (M_value (operand2 expression) st)))
      ((and (eq? '- (operator expression)) (binary-opp? expression))
       (-         (M_value (operand1 expression) st)
                  (M_value (operand2 expression) st)))
      ((and (eq? '* (operator expression)) (binary-opp? expression))
       (*         (M_value (operand1 expression) st)
                  (M_value (operand2 expression) st)))
      ((and (eq? '/ (operator expression))(binary-opp? expression))
       (quotient  (M_value (operand1 expression) st)
                  (M_value (operand2 expression) st)))
      ((and (eq? '% (operator expression))(binary-opp? expression))
       (remainder (M_value (operand1 expression) st)
                  (M_value (operand2 expression) st)))
      ((and (eq? '- (operator expression)) (unary-opp? expression))
       (-         (M_value (operand1 expression) st)))
      (else       (error 'badop "Invalid int expression ~s" (operator expression))))))



