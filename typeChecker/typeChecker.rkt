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


; lookup-op : (op) -> (or/c procedure? false/c)
; a function that extracts the definition of an operator or false
(define (lookup-op op)
  (if (eqv? #f (assoc op op-table))
      #f
  (second (assoc op op-table))))

; parse : s-expression -> Expr
(define (parse sexp)
  (cond
    [(list? sexp)
     (cond
         [(eq? 'with (first sexp)) (with
                  (first (second sexp))
                  (parse (second (second sexp)))
                  (parse (third sexp)))]
         [(not (eq? #f (lookup-op (first sexp))))
          (bin-num-op (lookup-op (first sexp)) (parse (second sexp)) (parse (third sexp)))]
         [(eq? 'iszero (first sexp)) (iszero (parse (second sexp)))]
         [(eq? 'bif (first sexp)) (bif (parse (second sexp)) (parse (third sexp)) (parse (fourth sexp)))]
         [(eq? 'ncons (first sexp)) (ncons (parse (second sexp)) (parse (third sexp)))]
         [(eq? 'nempty? (first sexp)) (isnempty (parse (second sexp)))]
         [(eq? 'nfirst (first sexp)) (nfirst (parse (second sexp)))]
         [(eq? 'nrest (first sexp)) (nrest (parse (second sexp)))]
         [(eq? 'fun (first sexp)) (fun
                                   (first (second sexp))
                                   (parse-type (third (second sexp)))
                                   (parse-type (fourth sexp))
                                   (parse (fifth sexp)))]
         [else (app (parse (first sexp)) (parse (second sexp)))]
         )]
    [(number? sexp) (num sexp)]
    [(eq? 'true sexp) (bool #t)]
    [(eq? 'false sexp) (bool #f)]
    [(eq? 'nempty sexp) (nempty)]
    [(symbol? sexp) (id sexp)]
    ))

(define (parse-type sexp)
  (case sexp
      [(boolean) (t-bool)]
      [(number) (t-num)]
      [(nlist) (t-nlist)]
      [else (t-fun (parse-type (first sexp)) (parse-type (third sexp)))]))
 
; type-of : Expr -> Type
; determines the type of an already parsed expression
(define (type-of e)
  (type-case Expr e
    [num (n) (t-num)]
    [bin-num-op (op lhs rhs)
                (if (and
                     (t-num? (type-of lhs))
                     (t-num? (type-of rhs)))
                    (t-num)
                    (error 'type-of "lhs and rhs must be number types"))]
    [bool (b) (t-bool)]
    [id (x) (error "unbound identifier")]
    [nempty () (t-nlist)]
    [ncons (first rest)
           (if (and
                 (t-num? (type-of first))
                 (t-nlist? (type-of rest)))
                (t-nlist)
                 (error 'type-of "first parameter of ncons must be a number and second parameter must be a list"))]
    [isnempty (list) (if (t-nlist? (type-of list))
                         (t-fun (t-nlist) (t-bool))
                         (error 'type-of "first parameter of nempty? must be a list"))]
    [nfirst (list) (if (t-nlist? (type-of list))
                       (t-fun (t-nlist) (t-num))
                       (error 'type-of "parameter of nfirst must be a list"))]
    [nrest (rest) (if (t-nlist? (type-of rest))
                       (t-fun (t-nlist) (t-nlist))
                       (error 'type-of "parameter of nrest must be a list"))]
    [else (error 'type-of "not implemented")]))

#|

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
(test (parse '(6 7)) (app (num 6) (num 7)))
; parse fun
(test (parse '(fun (x : number) : number (+ x x))) (fun
                                                    'x
                                                    (t-num)
                                                    (t-num)
                                                    (bin-num-op + (id 'x) (id 'x))))



; type-of tests
(test (type-of (parse '(+ 1 (* 3 (- 5 7))))) (t-num))

(test/exn (type-of (parse '(3 4))) "not implemented")
(test/exn (type-of (parse 'x)) "type error")


(parse-type '(number -> boolean))
(parse-type '(nlist -> boolean))
(parse-type '(nlist -> number))
(parse-type '((number -> boolean) -> boolean))

|#




; Expression: num
; * Is there an example of type-of on a correct num expression?
(test (type-of (parse 1)) (t-num))

; Expression: true
; * Is there an example of type-of on a correct true expression?
(test (type-of (parse 'true)) (t-bool))

; Expression: false
; * Is there an example of type-of on a correct false expression?
(test (type-of (parse 'false)) (t-bool))

; Expression: +
; * Is there an example of type-of on a correct + expression?
(test (type-of (parse '(+ 1 2))) (t-num))
; * Is there a test case for the lhs not being a number?
(test/exn (type-of (parse '(+ true 4))) "lhs and rhs must be number types")
; * Is there a test case for the rhs not being a number?
(test/exn (type-of (parse '(+ 4 true))) "lhs and rhs must be number types")

; Expression: -
; * Is there an example of type-of on a correct - expression?
(test (type-of (parse '(- 20 15))) (t-num))
; * Is there a test case for the lhs not being a number?
(test/exn (type-of (parse '(- true 15))) "lhs and rhs must be number types")
; * Is there a test case for the rhs not being a number?
(test/exn (type-of (parse '(- 20 true))) "lhs and rhs must be number types")

; Expression: *
; * Is there an example of type-of on a correct * expression?
(test (type-of (parse '(* 3 4))) (t-num))
; * Is there a test case for the lhs not being a number?
(test/exn (type-of (parse '(* true 4))) "lhs and rhs must be number types")
; * Is there a test case for the rhs not being a number?
(test/exn (type-of (parse '(* 3 true))) "lhs and rhs must be number types")



; Expression: iszero
; * Is there an example of type-of on a correct iszero expression?
; * Is there a test case for the input not being a number?

; Expression: bif
;  * Is there an example of type-of on a correct bif expression?
;  * Is there a test case for a non-boolean condition error?
;  * Is there a test case for a mismatch error?


; Expression: id
; * Is there an example of type-of on a correct id expression?
; * Is there a test case for a unbound identifier?

; Expression: with
; * Is there an example of type-of on a correct with expression?
; * Is there a test case for misuse of the identifier in the body?

; Expression: fun
; * Is there an example of type-of on a correct fun expression?
; * Is there a test case for misuse of the formal parameter in the body?
; * Is there a test case for a return-type mismatch error?

; Expression: app
; * Is there an example of type-of on a correct app expression?
; * Is there a test case for an operator that isn't a function?
; * Is there a test case for a wrong argument type?

; Expression: nempty
; * Is there an example of type-of on a correct nempty expression?
(test (type-of (parse 'nempty)) (t-nlist))

; Expression: ncons
; * Is there an example of type-of on a correct ncons expression?
(test (type-of (parse '(ncons 6 nempty))) (t-nlist))
; * Is there a test case for the first parameter not being a number?
(test/exn (type-of (parse '(ncons true nempty))) "first parameter of ncons must be a number and second parameter must be a list")
; * Is there a test case for the second parameter not being an nlist?

; Expression: nempty?
; * Is there an example of type-of on a correct nempty? expression?
(test (type-of (parse '(nempty? nempty))) (t-fun (t-nlist) (t-bool)))
; * Is there a test case for the input not being an nlist
(test/exn (type-of (parse '(nempty? 6))) "first parameter of nempty? must be a list")

; Expression: nfirst
; * Is there an example of type-of on a correct nfirst expression?
(test (type-of (parse '(nfirst nempty))) (t-fun (t-nlist) (t-num)))
(test (type-of (parse '(nfirst (ncons 6 nempty)))) (t-fun (t-nlist) (t-num)))
; * Is there a test case for the input not being an nlist?
(test/exn (type-of (parse '(nfirst 6))) "parameter of nfirst must be a list")

; Expression: nrest
; * Is there an example of type-of on a correct nrest expression?
(test (type-of (parse '(nrest (ncons 6 (ncons 7 nempty))))) (t-fun (t-nlist) (t-nlist)))
; * Is there a test case for the input not being an nlist?
(test/exn (type-of (parse '(nrest 13))) "parameter of nrest must be a list")




