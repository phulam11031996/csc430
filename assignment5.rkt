#lang typed/racket

(require typed/rackunit)

;; ------------------------------------------------------------------------- DATA DEFINITION
;; <ExprC-type> ::=
(define-type ExprC (U numC idC appC condC lamC))
;; <strC-type>
(struct strC ([n : String]) #:transparent) 
;; <numC-type>
(struct numC ([n : Real]) #:transparent) 
;; <idC-type>
(struct idC ([s : Symbol]) #:transparent)
;; <appC-type>
(struct appC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)
;; <lamC-type>
(struct lamC ([args : (Listof Symbol)] [body : ExprC]) #:transparent)
;; <condC-type>
(struct condC ([if : ExprC] [then : ExprC] [else : ExprC]) #:transparent)

;; <Binding-type> ::=
(struct Binding ([name : Symbol] [val : Value]) #:transparent)

;; <Value-type> ::=
(define-type Value (U closV Real Boolean String '+ '- '* '/ '<= 'equal? 'error))
;; <closV-type>
(struct closV ([args : (Listof Symbol)] [body : ExprC] [env : (Listof Binding)]) #:transparent)

;; <top-env> ::=
(define top-env (list
                 (Binding '+ '+)
                 (Binding '- '-)
                 (Binding '* '*)
                 (Binding '/ '/)
                 (Binding '<= '<=)
                 (Binding 'equal? 'equal?)
                 (Binding 'true true)
                 (Binding 'false false)))
;; -------------------------------------------------------------------------
                 

;; ------------------------------------------------------------------------- PARSE
(define (parse [program : Sexp]) : ExprC
  (match program 
    ;; <condC-parse>
    [(list 'if if 'then then 'else else) (condC (parse if) (parse then) (parse else))]
    ;; <numC-parse>
    [(? real? n) (numC n)]
    ;; <idC-parse>
    [(? symbol? s) (cond
                     [(valid-id? s) (idC s)]
                     [else (error 'IKEU "parse - identifiers cannot be a keyword")])]
    ;; <lamC-parse>
    [(list (list (? symbol? ids) ...) ': body) (cond
                                                 [(valid-arg-names? ids) (lamC (cast ids (Listof Symbol)) (parse body))]
                                                 [else (error 'IKEU "parse - symbols cannot be a keyword")])]
    ;; <let-parse>
    [(list 'let (list (list ids '= exprs) ...) body) (appC
                                                      (lamC (cast ids (Listof Symbol)) (parse body))
                                                      (map parse (cast exprs (Listof Sexp))))]
    ;; <appC-parse>
    [(list fun args ...) (appC (parse fun) (map parse args))]
    ;; <error-case>
    [other (error 'IKEU "parse - invalid syntax")]))
;; -------------------------------------------------------------------------


;; ------------------------------------------------------------------------- INTERP
(define (interp [expr : ExprC] [env : (Listof Binding)]) : Value
  (match expr
    ;; <numC-interp>
    [(numC n) n]
    ;; <idC-interp>
    [(idC id) (lookup id env)]
    ;; <lamC-interp>
    [(lamC a b) (closV (cast a (Listof Symbol)) b env)]
    ;; <condC-interp>
    [(condC if then else) (cond
                            [(equal? (interp if env) #t) (interp then env)]
                            [(equal? (interp if env) #f) (interp else env)]
                            [else (error 'IKEU "interp - condition needs to be a true or false value")])]
    ;; <appC-interp>
    [(appC f (list args ...)) (local ([define f-value (interp f env)]
                                      [define interped-args (map (lambda (arg) (interp (cast arg ExprC) env)) args)])
                                (cond
                                  [(and (top-env-binding? f-value) (equal? (length args) 2))
                                   (binop-eval f-value (first interped-args) (second interped-args))]
                                  [(closV? f-value)
                                   (interp (closV-body f-value) (append-bind (closV-args f-value) interped-args env))]
                                  [else (error 'IKEU "interp - not here yet")]))]))
;; -------------------------------------------------------------------------


;; ------------------------------------------------------------------------- TOP INTERP
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))
;; -------------------------------------------------------------------------


;; ------------------------------------------------------------------------- REQUIRE HELPERS
(define (serialize [val : Value]) : String
  (match val
    [(? real? val) (~v val)]
    [(? string? val) (~v val)]
    [#t "true"]
    [#f "false"]
    [(? closV? val) "#<procedure>"]
    [else "#<primop>"]))

(define (lookup [for : Symbol] [env : (Listof Binding)]) : Value
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [else (cond
            [(symbol=? for (Binding-name (first env)))
             (Binding-val (first env))]
            [else (lookup for (rest env))])]))

(define (binop-eval [s : Value] [l : Value] [r : Value]) : Value
  (cond
    [(equal? s 'equal?) (equal? l r)]
    [(and (real? l) (real? r))
     (match s
       ['+ (+ l r)]
       ['- (- l r)]
       ['* (* l r)]
       ['/ (/ l r)]
       ['<= (<= l r)])]
    [else (error 'IKEU "one argument was not a number")]))
;; ------------------------------------------------------------------------- 


;; ------------------------------------------------------------------------- HELPERS
(define (append-bind [s : (Listof Symbol)] [vals : (Listof Value)]  [env : (Listof Binding)]) : (Listof Binding)
  (match s
    ['() env]
    [(cons f r) (cons (Binding f (first vals)) (append-bind r (rest vals) env))]))

(define (valid-id? [s : Symbol]) : Boolean
  (match s
    [': #f]
    ['= #f]
    ['if #f]
    ['then #f]
    ['else #f]
    ['let #f]
    [other #t]))

(define (top-env-binding? [s : Any]) : Boolean
  (match s
    ['+ #t]
    ['- #t]
    ['* #t]
    ['/ #t]
    ['<= #t]
    ['equal? #t]
    ['true #t]
    ['false #t]
    ['error #t]
    [other #f]))

(define (keyword? [s : Symbol]) : Boolean
  (match s
    ['+ #t]
    ['- #t]
    ['/ #t]
    ['* #t]
    ['<= #t]
    ['= #t]
    [': #t]
    ['let #t]
    ['if #t]
    ['then #t]
    ['else #t]
    ['equal? #t]
    ['true #t]
    ['false #t]
    ['error #t]
    [other #f]))

(define (valid-arg-names? [params : (Listof Any)]) : Boolean
  (match params
    ['() #t]
    [(cons f r) (and (keyword? (cast f Symbol))) (valid-arg-names? r)]))
;; ------------------------------------------------------------------------- HELPERS

;; ------------------------------------------------------------------------- TESTS



