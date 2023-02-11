#lang racket
(require rackunit)

;; 2 Applying Lambdas
((lambda (x) (+ x 2)) 3)

((lambda (f g) (f (g 3)))
 (lambda (x) (+ x 3))
 (lambda (x) (* x 2)))

;; 3 curried-add
(define (curried-add a)
  (lambda (b) (+ a b)))

(check-equal? ((curried-add 5) 10) 15)
(check-equal? ((curried-add 20) 10) 30)

;; 4 curry2
(define (curry2 f)
  (lambda (x)
    (lambda (y)
      (f x y))))

(define add (lambda (x y) (+ x y)))
(define curried-add2 (curry2 add))
(check-equal? ((curried-add2 5) 6) 11)
(check-equal? ((curried-add2 7) 8) 15)

;; 5 curry3
(define (curry3 f)
  (lambda (x)
    (lambda (y)
      (lambda (z)
        (f x y z)))))

(define add3 (lambda (x y z) (+ x y z)))
(define curried-add3 (curry3 add3))
(check-equal? (((curried-add3 1) 2) 3) 6)
(check-equal? (((curried-add3 3) 3) 3) 9)

;; 6 contains?
(define (contains? los s)
  (match los
    ['() #f]
    [(cons f r) (or (equal? f s) (contains? s r))]
    [other #f]))


(check-equal? (contains? '(x y z) 'x) #t)
(check-equal? (contains? '(x y z) 'a) #f)



(define (in-list-many? lol loq)
  (map ((curry2 contains?) lol ) loq))

(define lol '(x y z))
(define loq '(a c))
(check-equal? (in-list-many? lol loq) '(#f #f))

#lang racket
(require rackunit)

;; 2 Applying Lambdas
((lambda (x) (+ x 2)) 3)

((lambda (f g) (f (g 3)))
 (lambda (x) (+ x 3))
 (lambda (x) (* x 2)))

;; 3 curried-add
(define (curried-add a)
  (lambda (b) (+ a b)))

(check-equal? ((curried-add 5) 10) 15)
(check-equal? ((curried-add 20) 10) 30)

;; 4 curry2
(define (curry2 f)
  (lambda (x)
    (lambda (y)
      (f x y))))

(define add (lambda (x y) (+ x y)))
(define curried-add2 (curry2 add))
(check-equal? ((curried-add2 5) 6) 11)
(check-equal? ((curried-add2 7) 8) 15)

;; 5 curry3
(define (curry3 f)
  (lambda (x)
    (lambda (y)
      (lambda (z)
        (f x y z)))))

(define add3 (lambda (x y z) (+ x y z)))
(define curried-add3 (curry3 add3))
(check-equal? (((curried-add3 1) 2) 3) 6)
(check-equal? (((curried-add3 3) 3) 3) 9)

;; 6 contains?
(define (contains? los s)
  (match los
    ['() #f]
    [(cons f r) (or (equal? f s) (contains? s r))]
    [other #f]))


(check-equal? (contains? '(x y z) 'x) #t)
(check-equal? (contains? '(x y z) 'a) #f)



(define (in-list-many? lol loq)
  (map ((curry2 contains?) lol ) loq))

(define lol '(x y z))
(define loq '(a c))
(check-equal? (in-list-many? lol loq) '(#f #f))

