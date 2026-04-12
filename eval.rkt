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
 M_value
 M_expression-vs
 M_boolean-vs
 M_value-vs
 make-vs
 vs-value
 vs-state
 register-funcall-handler!
 get-current-throw-handler
 set-current-throw-handler!)

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
      ((eq? e 'false) #t)
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

(define assignment-expression?
  (lambda (e)
    (and (list? e)
         (= (length e) 3)
         (eq? (operator e) '=)
         (symbol? (operand1 e)))))

(define funcall-expression?
  (lambda (e)
    (and (list? e)
         (pair? e)
         (eq? (car e) 'funcall))))

;; returns an error if a variable is unassigned
(define lookup-variable
  (lambda (exp st)
    (let ([v (state-lookup exp st)])
      (if (eq? v 'UNINITIALIZED)
          (error 'badop "Unassigned variable: ~s" exp)
          v))))

(define get-current-throw-handler
  (lambda ()
    current-throw-handler))

(define lang-bool->racket
  (lambda (v)
    (cond
      ((eq? v 'true) #t)
      ((eq? v 'false) #f)
      ((boolean? v) v)
      (else (error 'M_boolean "Expected boolean value, got: ~s" v)))))

(define make-vs
  (lambda (v s)
    (cons v s)))

(define vs-value car)
(define vs-state cdr)



(define M_expression
  (lambda (expression st)
    (vs-value (M_expression-vs expression st))))

(define M_boolean
  (lambda (expression st)
    (vs-value (M_boolean-vs expression st))))

(define M_value
  (lambda (expression st)
    (vs-value (M_value-vs expression st))))

;; ----------------------------------------------------------------------------------------

(define M_expression-vs
  (lambda (expression st)
    (cond
      ((assignment-expression? expression)
       (M_assignmentexpr-vs expression st))
      ((condition? expression)
       (M_boolean-vs expression st))
      (else
       (M_value-vs expression st)))))

;; assignment expression: (= x expr)
;; returns assigned value + updated state
(define M_assignmentexpr-vs
  (lambda (expression st)
    (let* ([rhs-vs (M_expression-vs (operand2 expression) st)]
           [rhs-val (vs-value rhs-vs)]
           [rhs-state (vs-state rhs-vs)]
           [new-state (state-update (operand1 expression) rhs-val rhs-state)])
      (make-vs rhs-val new-state))))

(define M_boolean-vs
  (lambda (expression st)
    (cond
      ((eq? expression 'true)
       (make-vs #t st))

      ((eq? expression 'false)
       (make-vs #f st))

      ((symbol? expression)
       (make-vs (lang-bool->racket (lookup-variable expression st)) st))

      ;; short-circuit &&
      ((and (binary-opp? expression) (eq? '&& (operator expression)))
       (let* ([left-vs (M_boolean-vs (operand1 expression) st)]
              [left-val (vs-value left-vs)]
              [st1 (vs-state left-vs)])
         (if left-val
             (M_boolean-vs (operand2 expression) st1)
             (make-vs #f st1))))

      ;; short-circuit ||
      ((and (binary-opp? expression) (eq? '|| (operator expression)))
       (let* ([left-vs (M_boolean-vs (operand1 expression) st)]
              [left-val (vs-value left-vs)]
              [st1 (vs-state left-vs)])
         (if left-val
             (make-vs #t st1)
             (M_boolean-vs (operand2 expression) st1))))

      ((and (unary-opp? expression) (eq? '! (operator expression)))
       (let* ([arg-vs (M_boolean-vs (operand1 expression) st)]
              [arg-val (vs-value arg-vs)]
              [st1 (vs-state arg-vs)])
         (make-vs (not arg-val) st1)))

      ((and (binary-opp? expression) (eq? '< (operator expression)))
       (let* ([left-vs (M_expression-vs (operand1 expression) st)]
              [left-val (vs-value left-vs)]
              [st1 (vs-state left-vs)]
              [right-vs (M_expression-vs (operand2 expression) st1)]
              [right-val (vs-value right-vs)]
              [st2 (vs-state right-vs)])
         (make-vs (< left-val right-val) st2)))

      ((and (binary-opp? expression) (eq? '<= (operator expression)))
       (let* ([left-vs (M_expression-vs (operand1 expression) st)]
              [left-val (vs-value left-vs)]
              [st1 (vs-state left-vs)]
              [right-vs (M_expression-vs (operand2 expression) st1)]
              [right-val (vs-value right-vs)]
              [st2 (vs-state right-vs)])
         (make-vs (<= left-val right-val) st2)))

      ((and (binary-opp? expression) (eq? '> (operator expression)))
       (let* ([left-vs (M_expression-vs (operand1 expression) st)]
              [left-val (vs-value left-vs)]
              [st1 (vs-state left-vs)]
              [right-vs (M_expression-vs (operand2 expression) st1)]
              [right-val (vs-value right-vs)]
              [st2 (vs-state right-vs)])
         (make-vs (> left-val right-val) st2)))

      ((and (binary-opp? expression) (eq? '>= (operator expression)))
       (let* ([left-vs (M_expression-vs (operand1 expression) st)]
              [left-val (vs-value left-vs)]
              [st1 (vs-state left-vs)]
              [right-vs (M_expression-vs (operand2 expression) st1)]
              [right-val (vs-value right-vs)]
              [st2 (vs-state right-vs)])
         (make-vs (>= left-val right-val) st2)))

      ((and (binary-opp? expression) (eq? '== (operator expression)))
       (let* ([left-vs (M_expression-vs (operand1 expression) st)]
              [left-val (vs-value left-vs)]
              [st1 (vs-state left-vs)]
              [right-vs (M_expression-vs (operand2 expression) st1)]
              [right-val (vs-value right-vs)]
              [st2 (vs-state right-vs)])
         (make-vs (equal? left-val right-val) st2)))

      ((and (binary-opp? expression) (eq? '!= (operator expression)))
       (let* ([left-vs (M_expression-vs (operand1 expression) st)]
              [left-val (vs-value left-vs)]
              [st1 (vs-state left-vs)]
              [right-vs (M_expression-vs (operand2 expression) st1)]
              [right-val (vs-value right-vs)]
              [st2 (vs-state right-vs)])
         (make-vs (not (equal? left-val right-val)) st2)))

      (else
       (error 'M_boolean "Invalid boolean expression ~s" expression)))))

(define M_value-vs
  (lambda (expression st)
    (cond
      ((number? expression)
       (make-vs expression st))

      ((symbol? expression)
       (make-vs (lookup-variable expression st) st))

      ((funcall-expression? expression)
       (M_funcall-expression expression st))

      ((and (eq? '+ (operator expression)) (binary-opp? expression))
       (let* ([left-vs (M_expression-vs (operand1 expression) st)]
              [left-val (vs-value left-vs)]
              [st1 (vs-state left-vs)]
              [right-vs (M_expression-vs (operand2 expression) st1)]
              [right-val (vs-value right-vs)]
              [st2 (vs-state right-vs)])
         (make-vs (+ left-val right-val) st2)))

      ((and (eq? '- (operator expression)) (binary-opp? expression))
       (let* ([left-vs (M_expression-vs (operand1 expression) st)]
              [left-val (vs-value left-vs)]
              [st1 (vs-state left-vs)]
              [right-vs (M_expression-vs (operand2 expression) st1)]
              [right-val (vs-value right-vs)]
              [st2 (vs-state right-vs)])
         (make-vs (- left-val right-val) st2)))

      ((and (eq? '* (operator expression)) (binary-opp? expression))
       (let* ([left-vs (M_expression-vs (operand1 expression) st)]
              [left-val (vs-value left-vs)]
              [st1 (vs-state left-vs)]
              [right-vs (M_expression-vs (operand2 expression) st1)]
              [right-val (vs-value right-vs)]
              [st2 (vs-state right-vs)])
         (make-vs (* left-val right-val) st2)))

      ((and (eq? '/ (operator expression)) (binary-opp? expression))
       (let* ([left-vs (M_expression-vs (operand1 expression) st)]
              [left-val (vs-value left-vs)]
              [st1 (vs-state left-vs)]
              [right-vs (M_expression-vs (operand2 expression) st1)]
              [right-val (vs-value right-vs)]
              [st2 (vs-state right-vs)])
         (make-vs (quotient left-val right-val) st2)))

      ((and (eq? '% (operator expression)) (binary-opp? expression))
       (let* ([left-vs (M_expression-vs (operand1 expression) st)]
              [left-val (vs-value left-vs)]
              [st1 (vs-state left-vs)]
              [right-vs (M_expression-vs (operand2 expression) st1)]
              [right-val (vs-value right-vs)]
              [st2 (vs-state right-vs)])
         (make-vs (remainder left-val right-val) st2)))

      ((and (eq? '- (operator expression)) (unary-opp? expression))
       (let* ([arg-vs (M_expression-vs (operand1 expression) st)]
              [arg-val (vs-value arg-vs)]
              [st1 (vs-state arg-vs)])
         (make-vs (- arg-val) st1)))

      (else
       (error 'badop "Invalid int expression ~s" expression)))))

;; Placeholder only.
;; Member 2 / later integration will replace this with real function-call execution.

(define current-throw-handler
  (lambda (val st)
    (error 'throw "No throw handler registered for expression context: ~a" val)))

(define set-current-throw-handler!
  (lambda (handler)
    (set! current-throw-handler handler)))

(define current-funcall-handler
  (lambda (expression st)
    (error 'M_funcall-expression
           "funcall handler not registered yet: ~s"
           expression)))

(define register-funcall-handler!
  (lambda (handler)
    (set! current-funcall-handler handler)))

(define M_funcall-expression
  (lambda (expression st)
    (current-funcall-handler expression st)))
