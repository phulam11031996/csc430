#lang typed/racket

(require typed/rackunit)

;; NOTE: Done with everything

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXPRC
;; <ExprC> :=
(define-type ExprC (U numC idC appC binopC leq0))
;; <numC-def>
(struct numC ([n : Real]) #:transparent)
;; <idC-def>
(struct idC ([s : Symbol]) #:transparent)
;; <appC-def>
(struct appC ([fun : Symbol] [arg : ExprC]) #:transparent)
;; <leq0?C>
(struct leq0 ([val : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
;; <binopC-def>
(struct binopC ([op : Symbol] [l : ExprC] [r : ExprC]) #:transparent)

;; <FunDefC>
(struct FunDefC ([name : Symbol] [arg : Symbol] [body : ExprC]) #:transparent)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PARSE
;; Header: to compute the AST for numC, idC, appC, binopC,
;; and, leq0 from a Sexp
;; parse : Sexp -> ExprC
(define (parse [program : Sexp]) : ExprC
  (match program 
    ;; <numC-parse>
    [(? real? n) (numC n)]
    ;; <idC-parse>
    [(? symbol? s) (cond
                     [(check-symbol? s) (error 'IKEU "invalid symbol")]
                     [else (idC s)])]
    ;; <appC-parse>
    [(list (? symbol? fun) arg) (cond
                                  [(check-symbol? fun) (error 'IKEU "invalid function name")]
                                  [else (appC fun (parse (cast arg Sexp)))])]
    ;; <binopC-parse>
    [(list (? symbol? op) l r) (cond
                                 [(check-symbol? op) (binopC op (parse l) (parse r))]
                                 [else (error 'IKEU "invalid operator")])]
    ;; <leq0-parse>
    [(list 'leq0? val 'then then 'else else) (leq0 (parse val) (parse then) (parse else))]
    ;; <error-case>
    [other (error 'IKEU "invalid syntax")]))


;; Header: to compute the AST for FunDef from Sexp
;; parse-fundef : Sexp -> FunDefC
(define (parse-fundef [program : Sexp]) : FunDefC
  (match program
    [(list 'fundef
           (? symbol? name)
           (? list? param)
           ':
           body) (cond
                   [(check-symbol? name) (error 'IKEU "invalid function name")]
                   [(not (equal? (length param) 1)) (error 'IKEU "invalid number of param")]
                   [(check-params? param) (error 'IKEU "invalid param name")]
                   [else (match body
                           [(? real? n) (FunDefC
                                         name
                                         (cast (first param) Symbol)
                                         (parse body))]
                           [(? list? body) (FunDefC
                                            name
                                            (cast (first param) Symbol)
                                            (parse (cast body Sexp)))])])]
    [other (error 'IKEU "invalid function syntax")]))


;; Header: to compute the list of AST from a list of FunDefC.
;; parse-prog will call the parse-funder to compute each FunDefC
;; parse-prog : Sexp -> (Listof FunDefC)
(define (parse-prog [funs : Sexp]) : (Listof FunDefC)
  (match funs
    ['() '()]
    [(cons f r) (cons (parse-fundef f) (parse-prog r))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INTERP
;; Header: to compute the answer/number from the AST/ExprC
;; interp : ExprC -> Real
(define (interp [e : ExprC] [fds : (Listof FunDefC)]) : Real
  (match e
    ;; <numC-interp>
    [(numC n) n]
    ;; <idC-interp>
    [(idC s) (cond
               [(equal? s 'init) 0]
               [else (error 'IKEU "shouldn't get here")])]
    ;; <appC-interp>
    [(appC f a) (local ([define fd (get-fundef f fds)])
                  (interp (subst (numC (interp a fds))
                                 (FunDefC-arg fd)
                                 (FunDefC-body fd))
                          fds))]
    ;; <binopC-interp>
    [(binopC op l r) (match op
                       ['+ (+ (interp l fds) (interp r fds))]
                       ['- (- (interp l fds) (interp r fds))]
                       ['* (* (interp l fds) (interp r fds))]
                       ['/ (cond
                             [(equal? 0 (interp r fds)) (error 'IKEU "divided by zero")]
                             [else (/ (interp l fds) (interp r fds))])])]
    ;; <leq0-interp>
    [(leq0 val then else) (cond
                            [(> (interp val fds) 0) (interp else fds)] 
                            [else (interp then fds)])]))


;; Header: to computer the answer/number fron a list of function/
;; FunDefC. interp-fns will look for the main method to compute the answer
;; interp-fns : (Listof FunDefC) -> Real
(define (interp-fns [funs : (Listof FunDefC)]) : Real
  (interp (find-main funs) funs))

;; Header: to combines parsing and evaluation
;; top-interp : Sexp -> Real
(: top-interp (Sexp -> Real))
(define (top-interp fun-sexps)
  (interp-fns (parse-prog fun-sexps))) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HELPERS
;; Header: to return true if a symbol is valid. Return false otherwise
;; valid-operator? : Symbol -> Boolean
(define (check-symbol? [s : Symbol]) : Boolean
  (match s
    ['+ #t]
    ['- #t]
    ['* #t]
    ['/ #t]
    ['fundef #t]
    ['leq0? #t]
    [other #f]))

;; Header:
;; check-param? : (Listof Symbol) -> Boolean
(define (check-params? [params : (Listof Sexp)]) : Boolean
  (match params
    ['() #t]
    [(cons f r) (and (check-symbol? (cast f Symbol)) (check-params? r))]))


;; Header: to replace a name in and expression with another expression and
;; return an ExprC from what/ExprC, for/Symbol, and in/ExprC
;; subt : ExprC Symbol ExprC -> ExprC
(define (subst [what : ExprC] [for : Symbol] [in : ExprC]) : ExprC
  (match in
    [(numC n) in]
    [(idC s) (cond
               [(symbol=? s for) what]
               [else in])]
    [(appC f a) (appC f (subst what for a))]
    [(binopC op l r) (binopC op (subst what for l) (subst what for r))]
    [(leq0 val then else) (leq0 (subst what for val) (subst what for then) (subst what for else))]))


;; Header: to get the FunDefC from a given function name, n,
;; and a list of FunDefC, fds.
;; get-fundef : Symbol (Listof FunDefC) -> FunDefC
(define (get-fundef [n : Symbol] [fds : (Listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'IKEU "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? n (FunDefC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))


;; Header: to run the program
;; find-main : (Listof FunDefC) -> ExprC
(define (find-main [funs : (Listof FunDefC)]) : ExprC
  (match funs
    ['() (error 'IKEU "no main function with init param")]
    [(cons (FunDefC name arg body) r) (cond
                                        [(and (equal? 'main name) (equal? 'init arg)) body]
                                        [else (find-main r)])]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TESTS
;; define some constants for testing
(define a-sexp-function '(fundef double {x} : {+ x x}))

(define list-of-Sexp '({fundef f {x} : {+ x 14}}
                       {fundef main {init} : {f 2}}))
(define list-of-FunDefC (list
                         (FunDefC 'f 'x (binopC '+ (idC 'x) (numC 14)))
                         (FunDefC 'main 'init (appC 'f (numC 2)))))
(define list-of-FunDefC-no-main (list
                                 (FunDefC 'f 'x (binopC '+ (idC 'x) (numC 14)))
                                 (FunDefC 'not-main 'init (appC 'f (numC 2)))))

;; check-symbol?
(check-equal? (check-symbol? '+) #t)
(check-equal? (check-symbol? '-) #t)
(check-equal? (check-symbol? '*) #t)
(check-equal? (check-symbol? '/) #t)
(check-equal? (check-symbol? 'fundef) #t)
(check-equal? (check-symbol? 'a) #f)

;; subst
(check-equal? (subst (numC 10) 'y (binopC '+ (idC 'x) (binopC '- (idC 'y) (idC 'x))))
              (binopC '+ (idC 'x) (binopC '- (numC 10) (idC 'x))))
(check-equal? (subst (numC 10) 'y (leq0 (numC 10) (idC 'y) (idC 'x)))
              (leq0 (numC 10) (numC 10) (idC 'x)))
(check-equal? (subst (numC 10) 'x (leq0 (numC 10) (idC 'y) (idC 'x)))
              (leq0 (numC 10) (idC 'y) (numC 10)))
(check-equal? (subst (numC 10) 'x (appC 'f (idC 'x)))
              (appC 'f (numC 10)))

;; get-fundef
(check-equal? (get-fundef 'f list-of-FunDefC) (FunDefC 'f 'x (binopC '+ (idC 'x) (numC 14))))
(check-equal? (get-fundef 'main list-of-FunDefC) (FunDefC 'main 'init (appC 'f (numC 2))))
(check-exn
 #px"reference to undefined function"
 (lambda () (get-fundef'nonexistfunction list-of-FunDefC)))

;; interp-fns
(check-equal? (interp-fns list-of-FunDefC) 16)
(check-exn
 #px"no main function with init param"
 (lambda () (interp-fns list-of-FunDefC-no-main)))

;; top-interp
(check-equal? (top-interp '{{fundef f {x} : {+ x 14}}
                            {fundef main {init} : {f 2}}}) 16)

;; interp
(check-equal? (interp (numC 1) list-of-FunDefC) 1)
(check-exn
 #px"shouldn't get here"
 (lambda () (interp (idC 'x) list-of-FunDefC)))
(check-equal? (interp (appC 'f (numC 2)) list-of-FunDefC) 16)
(check-equal? (interp (binopC '+ (numC 5) (numC 5)) list-of-FunDefC) 10)
(check-equal? (interp (binopC '- (numC 5) (numC 5)) list-of-FunDefC) 0)
(check-equal? (interp (binopC '* (numC 5) (numC 5)) list-of-FunDefC) 25)
(check-equal? (interp (binopC '/ (numC 5) (numC 5)) list-of-FunDefC) 1)
(check-exn
 #px"divided by zero"
 (lambda () (interp (binopC '/ (numC 5) (numC 0)) list-of-FunDefC)))
(check-equal? (interp (leq0 (numC 2) (numC 3) (numC 4)) list-of-FunDefC) 4)
(check-equal? (interp (leq0 (numC 0) (numC 2) (numC 3)) list-of-FunDefC) 2)

;; parse-prog
(check-equal? (parse-prog list-of-Sexp) list-of-FunDefC)

;; parse-fundef 
(define sexp-double-fun '(fundef double {x} : {+ x x}))
(define fundefc-double-fun (FunDefC 'double 'x (binopC '+ (idC 'x) (idC 'x))))
(check-equal? (parse-fundef sexp-double-fun) fundefc-double-fun)

(define sexp-invalid-func-name '(fundef fundef {x} : {+ 1 x}))
(check-exn
 #px"invalid function name"
 (lambda () (parse-fundef sexp-invalid-func-name)))

(define sexp-invalid-func-param '(fundef incre {x y} : {+ 1 x}))
(check-exn
 #px"invalid number of param"
 (lambda () (parse-fundef sexp-invalid-func-param)))

(define sexp-invalid-func-syntax '(fundef incre {x y} : : {+ 1 x}))
(check-exn
 #px"invalid function syntax"
 (lambda () (parse-fundef sexp-invalid-func-syntax)))

;; parse
(check-equal? (parse (list 'leq0? 2 'then 3 'else 4)) (leq0 (numC 2) (numC 3) (numC 4)))
(check-equal? (parse (list 'leq0? 0 'then 3 'else 4)) (leq0 (numC 0) (numC 3) (numC 4)))

(check-exn
 #px"invalid syntax"
 (lambda () (parse '{fundef 'main {'init} : {+ 'fundef 1}})))

(check-exn
 #px"invalid operator"
 (lambda () (parse (list '$ 1 2))))

(check-exn
 #px"invalid symbol"
 (lambda () (parse 'fundef)))

;; additional test
(check-equal? (top-interp
               (quote ((fundef minus-five (x) : (+ x (* -1 5)))
                       (fundef main (init) : (minus-five (+ 8 init)))))) 3)

(check-equal? (parse-fundef '(fundef f (x) : 13)) (FunDefC 'f 'x (numC 13)))

(check-exn
 #px"invalid param name"
 (lambda () (parse-fundef '(fundef f (+) : 13))))

(check-exn
 #px"invalid function name"
 (lambda () (parse-fundef '(fundef + (x) : 13))))

(check-exn
 #px"invalid symbol"
 (lambda () (parse 'leq0?)))

(check-exn
 #px"invalid function name"
 (lambda () (parse '(+ b))))

;; big test case
(check-equal? (interp-fns
               (parse-prog '{{fundef factorial {x} : {leq0? x then 1 else {* x {factorial {- x 1}}}}}
                             {fundef main {init} : {factorial 10}}}))
              3628800)
