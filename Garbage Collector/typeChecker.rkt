#lang plai

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


(define op-table
  (list
   (list '+ +)
   (list '- -)
   (list '* *)))


; lookup-op : (op) -> (or/c procedure? false/c)
; a function that extracts the definition of an operator or false
(define (lookup-op op)
  (if (eqv? #f (assoc op op-table))
      #f
  (second (assoc op op-table))))

; valid id
(define (is-valid-identifier? x)
  (and (symbol? x)
       (eq? #f (lookup-op x))
       ))

; valid bin-num-op
(define (is-valid-bin-num-op? x)
  (and (= 3 (length x)) (procedure? (lookup-op (first x)))))

(define (is-valid-iszero? sexp)
  (and (= 2 (length sexp)) (eq? 'iszero (first sexp))))

(define (is-boolean? bool)
  (or (eq? 'true bool) (eq? 'false bool)))

(define (get-bool bool)
  (cond
    [(eq? 'true bool) true]
    [(eq? 'false bool) false]))

(define (is-valid-bif? sexp)
  (and
   (= 4 (length sexp))
   (eq? 'bif (first sexp))))

(define (is-valid-ncons? sexp)
  (and
   (= 3 (length sexp))
   (eq? 'ncons (first sexp))))

(define (is-valid-nempty? sexp)
  (eq? 'nempty sexp))

(define (is-valid-isnempty sexp)
  (and
   (= 2 (length sexp))
   (eq? 'nempty? (first sexp))))

(define (is-valid-nfirst? sexp)
  (and
   (= 2 (length sexp))
   (eq? 'nfirst (first sexp))))

(define (is-valid-nrest? sexp)
  (and
   (= 2 (length sexp))
   (eq? 'nrest (first sexp))))


; valid with
(define (is-valid-with? sexp)
  (and
   (= 3 (length sexp)) ; checks if with statement has 3 items: with, list of bindings, and bodys
   (and
    (list? (second sexp))
    (= 2 (length (second sexp)))
    (is-valid-identifier? (first (second sexp)))) ; checks if the bindings is a list and if it is not empty
   ))

 
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
  (error 'type-of "not implemented"))

; parse num
(parse 6)
; parse id
(parse 'x)
(parse 'y)
; parse expression
(parse '(+ 1 2))
(parse '(* 3 4))
(parse '(- 17 5))
; parse boolean
(parse 'true)
(parse 'false)
; parse bif
(parse '(bif true 1 2))
; parse iszero
(parse '(iszero 0))
(parse '(ncons 6 6))
; parse nempty
(parse 'nempty)
; parse nfirst
(parse '(nfirst 6))
(parse '(nrest nempty))
; parse nempty?
(parse '(nempty? nempty))
; parse with
(parse '(with (x 6) (+ x x)))
