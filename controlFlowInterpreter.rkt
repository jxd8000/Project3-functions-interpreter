#lang racket
(require "simpleParser.rkt")

; An interpreter for a simple language that does if, while and assign.
; The language has code blocks and supports the unstructured flow control constructs: break and continue in a loop, and throwing an exception

; The main function.  Calls parser to get the parse tree and interprets it with a new state.
; If there is no return statement in the program, the returned value is the state
(define interpret
  (lambda (file)
    (scheme->language
     (M-state-statement-list (parser file) (newstate)
                             (lambda (s) s)    ; the "run next line of program" continuation
                             (lambda (v s) v)  ; the "return statement" continuation
                             (lambda (s) (myerror "Break used outside of loop"))
                             (lambda (s) (myerror "Continue used outside of loop"))
                             (lambda (e s) (myerror "Uncaught exception thrown"))))))

; interprets a list of statements.
(define M-state-statement-list
  (lambda (statement-list state next return break continue throw)
    (if (empty? statement-list)
        (next state)
        (M-state-statement (firststatement statement-list) state
                           (lambda (s) (M-state-statement-list (restofstatements statement-list) s next return break continue throw))
                           return break continue throw))))

; interpret a single statement 
(define M-state-statement
  (lambda (statement state next return break continue throw)
    (cond
      ((eq? (statement-type statement) 'return) (M-state-return (get-expression statement) state next return break continue throw))
      ((eq? (statement-type statement) 'var)    (if (exists-declare-value? statement)
                                                    (M-state-declare-and-assign (get-declare-var statement) (get-declare-value statement) state next return break continue throw)
                                                    (M-state-declare (get-declare-var statement) state next return break continue throw)))
      ((eq? (statement-type statement) '=)      (M-state-assign (get-assign-lhs statement) (get-assign-rhs statement) state next return break continue throw))
      ((eq? (statement-type statement) 'if)     (if (exists-else? statement)
                                                    (M-state-if-else (get-condition statement) (get-then statement) (get-else statement) state next return break continue throw)
                                                    (M-state-if (get-condition statement) (get-then statement) state next return break continue throw)))
      ((eq? (statement-type statement) 'while)  (M-state-while (get-condition statement) (get-loop-body statement) state next return break continue throw))
      ((eq? (statement-type statement) 'begin)  (M-state-block (get-block-list statement) state next return break continue throw))
      ((eq? (statement-type statement) 'try)    (if (empty? (get-finally statement))
                                                    (M-state-try-catch (get-try-body statement) (catch-var (get-catch statement)) (catch-body (get-catch statement)) state next return break continue throw)
                                                    (if (empty? (get-catch statement))
                                                        (M-state-try-finally (get-try-body statement) (finally-body (get-finally statement)) state next return break continue throw)
                                                        (M-state-try-catch-finally (get-try-body statement) (catch-var (get-catch statement)) (catch-body (get-catch statement)) (finally-body (get-finally statement)) state next return break continue throw))))
      ; The flow control "goto" statements:
      ((eq? (statement-type statement) 'continue) (continue state))
      ((eq? (statement-type statement) 'break)    (break state))
      ((eq? (statement-type statement) 'throw)    (throw (M-value (get-expression statement) state) state))
      (else (myerror "Unknown statement:" (statement-type statement))))))

; Intepret a return statement by immediately returning using a global return continuation
(define M-state-return
  (lambda (expression state next return break continue throw)
    (return (M-value expression state) state)))

; Interpret a declaration statement that does not assign a value
(define M-state-declare
  (lambda (variable state next return break continue throw)
    (next (add-binding variable 'novalue state))))

; Interpret a declaration statement that also assigns a value
(define M-state-declare-and-assign
  (lambda (variable value-expression state next return break continue throw)
    (next (add-binding variable (M-value value-expression state) state))))

; Interpret an assignment statement
(define M-state-assign
  (lambda (variable value-expression state next return break continue throw)
    (next (update-binding variable (M-value value-expression state) state))))

; Interpret an if statement that does not have an else statement
(define M-state-if
  (lambda (condition then-statement state next return break continue throw)
    (if (M-boolean condition state)
        (M-state-statement then-statement state next return break continue throw)
        (next state))))

; Interpret an if-else statement
(define M-state-if-else
  (lambda (condition then-statement else-statement state next return break continue throw)
    (if (M-boolean condition state)
        (M-state-statement then-statement state next return break continue throw)
        (M-state-statement else-statement state next return break continue throw))))

; Interpret a while loop.
(define M-state-while
  (lambda (condition body state next return break continue throw)
    (while-loop condition body state next return throw)))

; helper function to do the loop, lets us define a new break and continue continuation for this loop
(define while-loop
  (lambda (condition body state next return throw)
    (if (M-boolean condition state)
        (M-state-statement body state
                           (lambda (s) (while-loop condition body s next return throw))
                           return
                           next             ; the break jumps to the next line of code
                           (lambda (s) (while-loop condition body s next return throw))  ; the continue re-runs the loop
                           throw)
        (next state))))

; Interpret a block of code
(define M-state-block
  (lambda (statement-list state next return break continue throw)
    (M-state-statement-list statement-list
                            (push-frame state)
                            (lambda (s) (next (pop-frame s)))
                            (lambda (v s) (return v (pop-frame s)))
                            (lambda (s) (break (pop-frame s)))
                            (lambda (s) (continue (pop-frame s)))
                            (lambda (v s) (throw v (pop-frame s))))))

; interpret a block of code with an intial binding
(define interpret-block
  (lambda (statement-list var val state next return break continue throw)
    (M-state-statement-list statement-list
                            (add-binding var val (push-frame state))
                            (lambda (s) (next (pop-frame s)))
                            (lambda (v s) (return v (pop-frame s)))
                            (lambda (s) (break (pop-frame s)))
                            (lambda (s) (continue (pop-frame s)))
                            (lambda (v s) (throw v (pop-frame s))))))

; Interpret a try statement with a catch but no finally
(define M-state-try-catch
  (lambda (try-block catch-var catch-block state next return break continue throw)
    (let ((try-throw (lambda (ex s) (interpret-block catch-block catch-var ex s next return break continue throw))))
      (M-state-block try-block state next return break continue try-throw))))

; Interpret a try statement with both a catch and a finally
(define M-state-try-catch-finally
  (lambda (try-block catch-var catch-block finally-block state next return break continue throw)
    (let* ((new-return (lambda (v s) (M-state-block finally-block s (lambda (s2) (return v)) break continue throw)))
           (new-break (lambda (s) (M-state-block finally-block s (lambda (s2) (break s2)) return break continue throw)))
           (new-continue (lambda (s) (M-state-block finally-block s (lambda (s2) (continue s2)) return break continue throw)))
           (new-next (lambda (s) (M-state-block finally-block s (lambda (s2) (next s2)) return break continue throw)))
           (new-throw (lambda (e s) (M-state-block finally-block (lambda (s2) (throw e s2)) return break continue throw))) 
           (try-throw (lambda (ex s) (interpret-block catch-block catch-var ex s
                                                      new-next
                                                      new-return
                                                      new-break
                                                      new-continue
                                                      new-throw))))
      (M-state-block try-block state new-next new-return new-break new-continue try-throw))))

; Interpret a try statement with a finally and no catch
(define M-state-try-finally
  (lambda (try-block finally-block state next return break continue throw)
    (let* ((new-return (lambda (v s) (M-state-block finally-block s (lambda (s2) (return v)) break continue throw)))
           (new-break (lambda (s) (M-state-block finally-block s (lambda (s2) (break s2)) return break continue throw)))
           (new-continue (lambda (s) (M-state-block finally-block s (lambda (s2) (continue s2)) return break continue throw)))
           (new-next (lambda (s) (M-state-block finally-block s (lambda (s2) (next s2)) return break continue throw)))
           (new-throw (lambda (e s) (M-state-block finally-block s (lambda (s2) (throw e s2)) return break continue throw))))
      (M-state-block try-block state new-next new-return new-break new-continue new-throw))))

; Interprets an expression to produce an integer or boolean value.
; This routine checks for literals or variables and calls a helper function if there is an operator.
(define M-value
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((eq? 'true expression) #t)
      ((eq? 'false expression) #f)
      ((not (list? expression)) (lookup-binding expression state))
      (else (M-value-operator expression state)))))

; Inteprets a boolean expression in the current state
(define M-boolean M-value)

; Interprets an integer expression in the current state
(define M-integer M-value)

; Interprets an expression to produce an integer or boolean value.
; This routine checks for unary operators and calls a helper function if the expression is a binary operator.
(define M-value-operator
  (lambda (expression state)
    (let ((op (operator expression))
          (operand1-value (M-value (operand1 expression) state)))
      (cond
        ((eq? op '!) (not operand1-value))
        ((eq? op '-) (if (exists-operand2? expression)
                         (M-value-binary-op op operand1-value (operand2 expression) state)
                         (- operand1-value)))
        (else (M-value-binary-op op operand1-value (operand2 expression) state))))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define M-value-binary-op
  (lambda (operator operand1-value operand2-expression state)
    (cond
      ((eq? operator '+)  (+ operand1-value (M-value operand2-expression state)))
      ((eq? operator '-)  (- operand1-value (M-value operand2-expression state)))
      ((eq? operator '*)  (* operand1-value (M-value operand2-expression state)))
      ((eq? operator '/)  (quotient operand1-value (M-value operand2-expression state)))
      ((eq? operator '%)  (remainder operand1-value (M-value operand2-expression state)))
      ((eq? operator '==) (isequal operand1-value (M-value operand2-expression state)))
      ((eq? operator '!=) (not (isequal operand1-value (M-value operand2-expression state))))
      ((eq? operator '<)  (< operand1-value (M-value operand2-expression state)))
      ((eq? operator '>)  (> operand1-value (M-value operand2-expression state)))
      ((eq? operator '<=) (<= operand1-value (M-value operand2-expression state)))
      ((eq? operator '>=) (>= operand1-value (M-value operand2-expression state)))
      ((eq? operator '||) (or operand1-value (M-value operand2-expression state)))
      ((eq? operator '&&) (and operand1-value (M-value operand2-expression state)))
      (else (myerror "Unknown operator:" operator)))))

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))


;-----------------
; HELPER FUNCTIONS
;-----------------

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define firststatement car)
(define restofstatements cdr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

; these helper functions define the parts of the various statement types
(define statement-type operator)
(define get-expression operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-loop-body operand2)
(define exists-else? exists-operand3?)
(define get-block-list cdr)
(define get-try-body operand1)
(define get-catch operand2)
(define get-finally operand3)

(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))

(define catch-body operand2)
(define finally-body operand1)


;------------------------
; State Functions: A state is a list of names followed by a list of the values bound to each associated name
;   The state is organized in "frames" corresponding to scope levels.
;------------------------

; create a new empty state
(define newstate
  (lambda ()
    (list (newstateframe))))

; creates a new frame for a state
(define newstateframe
  (lambda ()
    '(()())))

; add a frame onto the top of the state
(define push-frame
  (lambda (state)
    (cons (newstateframe) state)))

; remove a frame from the top of the state
(define pop-frame
  (lambda (state)
    (cdr state)))

; some abstraction functions to access frames
(define topframe car)
(define remainingframes cdr)
(define noframes? null?)

; does a binding for a given variable exist in the state?
(define exists-in-state?
  (lambda (var state)
    (cond
      ((noframes? state) #f)
      ((exists-in-list? var (variables (topframe state))) #t)
      (else (exists-in-state? var (remainingframes state))))))

; does a variable exist in a list of variables?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

; Looks up a value in the state.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup-binding
  (lambda (var state)
    (cond
      ((noframes? state) (myerror "error: undefined variable" var))
      ((exists-in-list? var (variables (topframe state))) (get-value var (lookup-in-frame var (topframe state))))
      (else (lookup-binding var (remainingframes state))))))

; A helper function that converts a stored value: it gives an error if the stored value is 'no-value, and it converts 'true to #t and 'false to #f
(define get-value
  (lambda (var value)
    (if (eq? 'novalue value)
        (myerror "error: variable without an assigned value:" var)
        (language->scheme value))))

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (myerror "error: undefined variable" var))
      (else (get-value-at-index (indexof var (variables frame)) (values-store frame))))))

; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; Get the value stored at a given index in the list
(define get-value-at-index
  (lambda (n l)
    (cond
      ((zero? n) (car l))
      (else (get-value-at-index (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the state.  Gives an error if the variable already exists in this frame.
(define add-binding
  (lambda (var val state)
    (if (exists-in-list? var (variables (topframe state)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (topframe state)) (remainingframes state)))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (scheme->language val) (values-store frame)))))

; Changes the binding of a variable to a new value in the state.  Gives an error if the variable does not exist.
(define update-binding
  (lambda (var val state)
    (if (exists-in-state? var state)
        (update-existing-binding var val state)
        (myerror "error: variable used but not defined:" var))))

; Changes the binding of a variable in the state to a new value
(define update-existing-binding
  (lambda (var val state)
    (if (exists-in-list? var (variables (topframe state)))
        (cons (update-in-frame var val (topframe state)) (remainingframes state))
        (cons (topframe state) (update-existing-binding var val (remainingframes state))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (values-store frame)))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((eq? var (car varlist)) (cons (scheme->language val) (cdr vallist)))
      (else (cons (car vallist) (update-in-frame-store var val (cdr varlist) (cdr vallist)))))))

; Returns the list of variables from a state
(define variables
  (lambda (state)
    (car state)))

; Returns the list of values stored from a state
(define values-store
  (lambda (state)
    (cadr state)))

; Functions to convert the Scheme #t and #f to our languages true and false, and back.

(define language->scheme
  (lambda (v) 
    (cond 
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))



; I create my own error function to respond to syntax errors nicely, and that should work across multiple racket/scheme versions
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (str vals)
                        (if (null? vals)
                            str
                            (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))

