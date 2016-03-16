#lang plai

; type definitions

(define-type Expr
  [num (n number?)]
  [id (v symbol?)]
  [bool (b boolean?)]
  [bin-num-op (op procedure?) (lhs Expr?) (rhs Expr?)]
  [iszero (e Expr?)]
  [bif (test Expr?) (then Expr?) (else Expr?)]
  [with (bound-id symbol?) (bound-body Expr?) (body Expr?)]
  [fun (arg-id symbol?)
       (arg-type Type?) (result-type Type?)
       (body Expr?)]
  [app (fun-expr Expr?) (arg-expr Expr?)]
  [nempty]
  [ncons (first Expr?) (rest Expr?)]
  [nfirst (e Expr?)]
  [nrest (e Expr?)]
  [isnempty (e Expr?)])
 
(define-type Type
  [t-num]
  [t-bool]
  [t-nlist]
  [t-fun (arg Type?) (result Type?)])

; table of binary number operations
(define op-table
  (list
   (list '+ +)
   (list '- -)
   (list '* *)))

; list of key words that can't be used as identifier

(define key-word-table
  (list 'true 'false 'iszero 'bif 'with 'fun
        'nempty 'ncons 'empty? 'nfirst 'nrest 'app))


; lookup-op : (op) -> (or/c procedure? false/c)
; a function that extracts the definition of an operator or false
(define (lookup-op op)
  (if (eqv? #f (assoc op op-table))
      #f
  (second (assoc op op-table))))

(define (lookup-keyword keyword)
  (ormap (lambda (x) (eq? keyword x)) key-word-table))

; valid id
(define (is-valid-identifier? id)
  (and (symbol? id)
       (eq? #f (lookup-keyword id))
       ))

; valid bin-num-op expression
(define (is-valid-bin-num-op? x)
  (and (= 3 (length x)) (procedure? (lookup-op (first x)))))

; valid iszero expression
(define (is-valid-iszero? sexp)
  (and (= 2 (length sexp)) (eq? 'iszero (first sexp))))

; valid bool expression
(define (is-boolean? bool)
  (or (eq? 'true bool) (eq? 'false bool)))

; gets the boolean
(define (get-bool bool)
  (cond
    [(eq? 'true bool) true]
    [(eq? 'false bool) false]))

; valid bif expression
(define (is-valid-bif? sexp)
  (and
   (= 4 (length sexp))
   (eq? 'bif (first sexp))))

; valid ncons expression
(define (is-valid-ncons? sexp)
  (and
   (= 3 (length sexp))
   (eq? 'ncons (first sexp))))

; valid nempty expression
(define (is-valid-nempty? sexp)
  (eq? 'nempty sexp))

; valid isnempty expresion
(define (is-valid-isnempty sexp)
  (and
   (= 2 (length sexp))
   (eq? 'nempty? (first sexp))))

; valid nfirst expression
(define (is-valid-nfirst? sexp)
  (and
   (= 2 (length sexp))
   (eq? 'nfirst (first sexp))))
; valid nrest expression
(define (is-valid-nrest? sexp)
  (and
   (= 2 (length sexp))
   (eq? 'nrest (first sexp))))


; valid with expression
(define (is-valid-with? sexp)
  (and
   (= 3 (length sexp)) ; checks if with statement has 3 items: with, list of bindings, and bodys
   (and
    (list? (second sexp))
    (= 2 (length (second sexp)))
    (is-valid-identifier? (first (second sexp)))) ; checks if the bindings is a list and if it is not empty
   ))

; valid app expression
(define (is-valid-app? sexp)
  (and
   (list? sexp)
   (not (empty? sexp))
   (= 3 (length sexp))
   (eq? 'app (first sexp))))

 
; parse : s-expression -> Expr
(define (parse sexp)
  (cond
    [(list? sexp)
     (cond
       [(is-valid-with? sexp) (with
                               (first (second sexp))
                               (parse (second (second sexp)))
                               (parse (third sexp)))]
       [(is-valid-bin-num-op? sexp)
        (bin-num-op (lookup-op (first sexp)) (parse (second sexp)) (parse (third sexp)))]
       [(is-valid-iszero? sexp) (iszero (parse (second sexp)))]
       [(is-valid-bif? sexp) (bif (parse (second sexp)) (parse (third sexp)) (parse (fourth sexp)))]
       [(is-valid-ncons? sexp) (ncons (parse (second sexp)) (parse (third sexp)))]
       [(is-valid-isnempty sexp) (isnempty (parse (second sexp)))]
       [(is-valid-nfirst? sexp) (nfirst (parse (second sexp)))]
       [(is-valid-nrest? sexp) (nrest (parse (second sexp)))]
       [(is-valid-app? sexp)
        (app (parse (second sexp))
             (parse (third sexp)))]
       [else (error 'parse "Illegal syntax")]
      )]
    [(number? sexp) (num sexp)]
    [(is-valid-nempty? sexp) (nempty)]
    [(is-boolean? sexp) (bool (get-bool sexp))]
    [(is-valid-identifier? sexp) (id sexp)]
    [else (error 'parse "Illegal syntax")]
    ))
 
; type-of : Expr -> Type
(define (type-of e)
  (type-case Expr e
    [num (n) (t-num)]
    [bin-num-op (op lhs rhs)
                (if (and
                     (t-num? (type-of lhs))
                     (t-num? (type-of rhs)))
                    (t-num)
                    (error 'type-of "error"))]
    [bool (b) (t-bool)]
    [else (error 'type-of "not implemented")]))



; parse tests

; parse num
(test (parse 6) (num 6))
; parse id
(test (parse 'x) (id 'x))
(test (parse 'y) (id 'y))
; parse expression
(test (parse '(+ 1 2)) (bin-num-op + (num 1) (num 2)))
(test (parse '(* 3 4)) (bin-num-op * (num 3) (num 4)))
(test (parse '(- 17 5)) (bin-num-op - (num 17) (num 5)))
; parse boolean
(test (parse 'true) (bool #t))
(test (parse 'false) (bool #f))
; parse bif
(test (parse '(bif true 1 2)) (bif (bool #t) (num 1) (num 2)))
; parse iszero
(test (parse '(iszero 0)) (iszero (num 0)))
; parse ncons
(test (parse '(ncons 6 6)) (ncons (num 6) (num 6)))
(test (parse '(ncons (nfirst 6) (nrest nempty)))
      (ncons (nfirst (num 6)) (nrest (nempty))))
(test (parse '(ncons (nfirst 6)
                     (nrest
                      (ncons (nfirst 3)
                            (nrest nempty)))))
      (ncons
       (nfirst (num 6))
       (nrest
        (ncons
         (nfirst (num 3))
         (nrest (nempty))))))

; parse nempty
(test (parse 'nempty) (nempty))
; parse nfirst
(test (parse '(nfirst 6)) (nfirst (num 6)))
(test (parse '(nfirst nempty)) (nfirst (nempty)))
(test (parse '(nrest nempty)) (nrest (nempty)))
; parse nempty?
(test (parse '(nempty? nempty)) (isnempty (nempty)))
; parse with
(test (parse '(with (x 6) (+ x x)))
      (with 'x (num 6) (bin-num-op + (id 'x) (id 'x))))
; parse app
(test (parse '(app 6 7)) (app (num 6) (num 7)))




; type-of tests
(test (type-of (parse 1)) (t-num))
(test (type-of (parse 'true)) (t-bool))
(test (type-of (parse 'false)) (t-bool))
(test (type-of (parse '(+ 1 2))) (t-num))
(test (type-of (parse '(* 3 4))) (t-num))
(test (type-of (parse '(- 20 15))) (t-num))
(test (type-of (parse '(+ 1 (* 3 (- 5 7))))) (t-num))
