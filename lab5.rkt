#lang racket
(require typed/rackunit)

;; 0
;; solution
(define p1 (lambda (v) (lambda (f) v)))

(check-equal? ((p1 8) (lambda (x) 1234)) 8)


;; 1 Warmup
;; Header: a function called one that accepts a
;; function and an argument and applies the function
;; to the argument
;; one : ask professor for this
(define one (lambda (x) (lambda (y) (x y))))

(define double (lambda (x) (+ x x)))
(define triple (lambda (x) (+ x x x)))

(check-equal? ((one double) 5) 10)
(check-equal? ((one triple) 5) 15)


;; 2 Continuing on
;; 1
;; Header: a function called two that accepts a
;; function and an argument and applies the function
;; to the result of applying the function to the argument.
;; two : ask professor for this
(define two (lambda (x) (lambda (y) (x (x y)))))

(check-equal? ((two double) 5) 20)
(check-equal? ((two triple) 5) 45)

;; 2
;; Header: a function called zero that accepts a
;; function and an argument and returns the argument.
(define zero (lambda (x) (lambda (y) y)))

(check-equal? ((zero double) 5) 5)
(check-equal? ((zero triple) 5) 5)

;; 3
;; Header: a function called add1 that accepts a
;; number-like function and returns a new number-like 
;; function that does the function "one more time".
(define add1 (lambda (nl) (lambda (fun) (lambda (x) (fun ((nl fun) x))))))


(check-equal? (((add1 two) double) 5) 40)
(check-equal? (((add1 two) triple) 5) 135)

;; 4
;; Header: a function called ’add’ that accepts two
;; functions like zero and one and returns a function
;; that applies its first argument to its second
;; argument a number of times that corresponds to the
;; sum of the two ’numbers’ it was given.
(define add (lambda (nl1) (lambda (nl2) (lambda (f) (lambda (x) ((nl1 f) ((nl2 f) x)))))))

(check-equal? ((((add zero) one) double) 5) 10)
(check-equal? ((((add two) one) double) 5) 40)
(check-equal? ((((add two) zero) double) 5) 20)
(check-equal? ((((add zero) two) double) 5) 20)

;; 5
;; Header:  a function called tru that accepts two
;; arguments and returns the first one.
(define tru (lambda (x) (lambda (y) x)))

(check-equal? ((tru 1) 2) 1)
(check-equal? ((tru double) triple) double)
(check-equal? ((tru triple) double) triple)

;; 6
;; Header: a function called fals that accepts two
;; arguments and returns the second one.
(define fals (lambda (x) (lambda (y) y)))

(check-equal? ((fals 1) 2) 2)
(check-equal? ((fals double) triple) triple)
(check-equal? ((fals triple) double) double)

;; 7
;; Header: a function called ’if’ that accepts three
;; arguments. If the first one turns out to be the
;; function tru (as above), it returns the result of
;; the second argument. If the first one turns out to
;; be the function fals (as above), it returns the
;; result of the third argument.
(define if (lambda (f) (lambda (x) (lambda (y) ((f x) y)))))

(check-equal? (((if tru) 1) 2) 1)
(check-equal? (((if fals) 1) 2) 2)
(check-equal? (((if tru) double) triple) double)
(check-equal? (((if fals) double) triple) triple)

;; 8
;; Header: a program in IKEU5 that includes the definitions
;; of ‘add‘ and ‘one‘ and ‘two‘ then uses ‘add‘ and ‘two‘
;; and ‘one‘ to produce ‘three‘, then applies it to a
;; function that doubles a number, and checks
;; that the result is correct.
(module sim-IKEU5 racket
       (provide
        [rename-out (#%lam-app #%app)
                    (my-let let)
                    (my-if if)]
        #%module-begin
        #%datum
        + - * / = equal? <=)
       (define-syntax (#%lam-app stx)
         (syntax-case stx (:)
           [(_ (args ...) : body)
            #'(lambda (args ...) body)]
           [(_ e ...)
            #'(#%app e ...)]))
       (define-syntax my-if
         (syntax-rules (then)
           [(if e1 then e2 else e3)
            (if e1 e2 e3)]))
       (define-syntax my-let
         (syntax-rules ()
           [(let ([v = e] ...) eb)
            ((lambda (v ...) eb) e ...)])))

;;(define one (lambda (x) (lambda (y) (x y))))

(module my-module (submod ".." sim-IKEU5)
  (let ([add = {{nl1} : {{nl2} : {{f} : {{x} : {{nl1 f} {{nl2 f} x}}}}}}]
        [one = {{f} : {{x} : {f x}}}]
        [two = {{f} : {{x} : {f {f x}}}}]
        [double = {{x} : {+ x x}}]
        [three = {{add} : {{two} : {{one} : {{f} : {{{add two} one} f}}}}}])
    ;(double 2))
    ;((one double) 3))
    ;((two double) 3))
    ;((((add one) two) double) 3))
    (((((three add) two) one) double) 3)) ; Return 24
  )
(require 'my-module)


    
