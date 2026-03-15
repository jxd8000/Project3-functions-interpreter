#lang racket
(require "../state.rkt")
(displayln "state.rkt loaded OK")

; empty-state
(define st0 (empty-state))
(displayln st0) ; '()
(displayln "empty-state test done")

; state-has
(define st '((y 99) (x 10)))
(displayln (state-has? 'x st)) ; #t
(displayln (state-has? 'z st)) ; #f
(displayln "state-has test done")

; state-declare
(define st1 (state-declare 'x st0))
(displayln st1)
(displayln (state-has? 'x st1))
(displayln "state-declare test done")

; state-declare/init
(define st2 (state-declare/init 'y 99 st1))
(displayln st2)
(displayln (state-has? 'y st2))      ; #t
(displayln (state-lookup 'y st2))    ; 99
(displayln "state-declare/init test done")

; state-lookup
(displayln (state-lookup 'x st2)) ; UNINITIALIZED
(displayln "state-lookup test done")

; state-update
(define st3 (state-update 'x 10 st2))
(displayln st3)
(displayln (state-lookup 'x st3)) ; 10
(displayln "state-update test done")

; state-set-return
(define st11 (state-set-return 7 st0))     ; st0 is empty-state
(displayln (state-get-return st11))        ; 7
(define st22 (state-set-return 42 st11))   ; set again should update, not duplicate
(displayln (state-get-return st22))        ; 42
(displayln "state-set-return test done")

; state-get-return
(define st111 (state-declare/init 'x 10 st22))
(define st222 (state-set-return 99 st111))
(displayln (state-lookup 'x st222))          ; 10
(displayln (state-get-return st222))         ; 99
(displayln "state-get-return test done")

(displayln "STATE TESTS DONE!")