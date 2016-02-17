#lang plai

(define-type Binding
  [binding (name symbol?) (named-expr CFWAE?)])
 
(define-type CFWAE
  [num (n number?)]
  [binop (op procedure?) (lhs CFWAE?) (rhs CFWAE?)]
  [with (lob (listof Binding?)) (body CFWAE?)]
  [id (name symbol?)]
  [if0 (c CFWAE?) (t CFWAE?) (e CFWAE?)]
  [fun (args (listof symbol?)) (body CFWAE?)]
  [app (f CFWAE?) (args (listof CFWAE?))])
 
(define-type Env
  [mtEnv]
  [anEnv (name symbol?) (value CFWAE-Value?) (env Env?)])
 
(define-type CFWAE-Value
  [numV (n number?)]
  [closureV (params (listof symbol?))
            (body CFWAE?)
            (env Env?)])



(define op-table
  (list (list '+ +) (list '- -) (list '* *) (list '/ /) (list 'with with)))


; lookup-op : (op) -> (or/c procedure? false/c)
; a function that extracts the definition of an operator or false
(define (lookup-op op)
  (if (eqv? #f (assoc op op-table))
      #f
  (second (assoc op op-table))))



;; helper functions for parser
;-----------------------------------------------------------------
;-----------------------------------------------------------------
;-----------------------------------------------------------------
;-----------------------------------------------------------------

; valid id
(define (is-valid-identifier? x)
  (and (symbol? x)
       (eq? #f (lookup-op x))
       (not (eq? x 'if0))
       (not (eq? x 'fun))
       ))

; valid binop
(define (is-valid-binop? x)
  (and (= 3 (length x)) (procedure? (lookup-op (first x)))))

; valid with
(define (is-valid-with? x)
  (and
   (= 3 (length x)) ; checks if with statement has 3 items: with, list of bindings, and bodys
   (and (list? (second x)) (not (empty? (second x)))) ; checks if the list of bindings is a list and if it is not empty
   (andmap
    (lambda (y)
      (and
       (list? y)
       (= 2(length y))
       (is-valid-identifier? (first y)))) (second x))
   ))


; valid if0
(define (is-valid-if0? if-exp)
  (and (eq? (first if-exp) 'if0)
       (= 4 (length if-exp))))


;functions used to find if duplicate identifiers in fun
(define (is-in-id-list? id id-list)
    (ormap
   (lambda (x)
     (symbol=? id x))
   id-list))

(define (duplicate-ids-in-fun? id-list)
  (foldr
   (lambda (a b)
           (if (is-in-id-list? a b)
               (error 'parse "Duplicate identifiers")
               (cons a b)
                ))
   empty
   id-list))

;functions used to find if there are multiple bindings---------------------------------------------
(define (is-in-binding-list? bind lst)
  (ormap
   (lambda (x)
     (symbol=? (binding-name x) (binding-name bind)))
   lst))

(define (check-multiple-bindings bindings)
  (foldr
   (lambda (a b)
           (if (is-in-binding-list? a b)
               (error 'parse "Duplicate identifiers")
               (cons a b)
                ))
   empty
   bindings))
;-----------------------------------------------------------------------------------------------------------
; valid function
(define (is-valid-fun? fun-exp)
  (and (= 3 (length fun-exp))
       (list? (second fun-exp))
       (andmap symbol? (second fun-exp))))



;; parse : (s-exp) -> CFWAE
;; Parses s-exp into a CFWAE
(define (parse s-exp)
  (cond
    [(number? s-exp) (num s-exp)]
    [(list? s-exp)
     (cond
       [ (empty? s-exp) (error 'parse "Illegal syntax")]
       [ (eqv? with (lookup-op (first s-exp)))
         (if (is-valid-with? s-exp)
             (with
              (check-multiple-bindings
               (map
                (lambda (x)
                  (binding (first x) (parse (second x)))) (second s-exp)))
              (parse (third s-exp)))
             (error 'parse "Illegal syntax"))]
       [(eq? (first s-exp) 'if0)
        (if (is-valid-if0? s-exp)
            (if0 (parse (second s-exp)) (parse (third s-exp)) (parse (fourth s-exp)))
            (error 'parse "Illegal syntax"))]
       [(is-valid-binop? s-exp)
        (binop (lookup-op (first s-exp)) (parse (second s-exp)) (parse (third s-exp)))]
       [(eq? (first s-exp) 'fun)
        (if (is-valid-fun? s-exp)
            (fun
             (duplicate-ids-in-fun? (second s-exp))
             (parse (third s-exp)))
            (error 'parse "Illegal syntax"))]
       [else  (app
               (parse (first s-exp))
               (foldr
                (lambda (a b)
                  (cons (parse a) b))
                empty
                (rest s-exp)))]
       )]
     [(is-valid-identifier? s-exp)
      (id s-exp)]
    [else (error 'parse "Illegal syntax")]))






;; test cases for lookup-op function
(test (lookup-op '+) +)
(test (lookup-op '-) -)
(test (lookup-op '*) *)
(test (lookup-op '/) /)
(test (lookup-op 'with) with)

;; test cases for rinterp evaluation

; Feature: literals
; an example of parsing a number expression
(test (parse '12) (num 12))
;a test case for a literal that is not a number
(test/exn (parse "Test Case") "Illegal syntax")


; Feature: binary operators
; an example of parsing a + expression properly?
(test (parse '(+ 1 2)) (binop + (num 1) (num 2)))
; an example of parsing a - expression properly?
(test (parse '(- 10 7)) (binop - (num 10) (num 7)))
; an example of parsing a * expression properly?
(test (parse '(* 3 4)) (binop * (num 3) (num 4)))
; an example of parsing a / expression properly?
(test (parse '(/ 100 10)) (binop / (num 100) (num 10)))
;a test case for: too many pieces
(test/exn (parse '(+ 1 2 3 4 5 6 7)) "Illegal syntax")
;Is there a test case for: too few pieces
(test/exn (parse '(* 27)) "Illegal syntax")



; Feature: with
; an example of parsing a with expression properly
(test (parse '(with ([x 5]) (+ 1 2))) (with (list (binding 'x (num 5))) (binop + (num 1) (num 2))))
; a test case for: too few pieces in the expression 
(test/exn (parse '(with ((y 12)))) "Illegal syntax")
; a test case for: too many pieces in the expression
(test/exn (parse '(with ([x 1]) (/ 100 20) (+ 1 2))) "Illegal syntax")
; a test case for: invalid bindings list (not a list) 
(test/exn (parse '(with [x 1] (+ 1 2))) "Illegal syntax")
; a test case for: invalid binding within the bindings (not a list)? 
(test/exn (parse '(with (x 5) (+ 1 x) (+ 2 x))) "Illegal syntax")
; a test case for: invalid binding (too few pieces) 
(test/exn (parse '(with ([z]) (/ z 100))) "Illegal syntax")
; a test case for: invalid binding (too many pieces)? 
(test/exn (parse '(with ([x 4 3]) (* x x))) "Illegal syntax")
; Is there a test case for: invalid binding (first item not a symbol)?
(test/exn (parse '(with ([3 12]) (+ 1 x))) "Illegal syntax")

; Feature: id
; an example of parsing a id expression properly
(test (parse 'x) (id 'x))
(test (parse '(+ x x)) (binop + (id 'x) (id 'x)))
; a test case for: not an id (+)
(test/exn (parse '+) "Illegal syntax")
; a test case for: not an id (-)
(test/exn (parse '-) "Illegal syntax")
; a test case for: not an id (*)
(test/exn (parse '*) "Illegal syntax")
; a test case for: not an id (/)
(test/exn (parse '/) "Illegal syntax")
; a test case for: not an id (with)
(test/exn (parse 'with) "Illegal syntax")

; Other:
; a test case for an expression with no operator (an empty list) 
(test/exn (parse '()) "Illegal syntax")


;----------------------------------------------------------------------------------------------------------
; new tests for this lab
(test (parse '(if0 0 (+ 1 2) (+ 3 4))) (if0 (num 0) (binop + (num 1) (num 2)) (binop + (num 3) (num 4))))
(test (parse '(if0 x x x)) (if0 (id 'x) (id 'x) (id 'x)))
(test/exn (parse '(if0 (+ 1 2) (+ 3 4))) "Illegal syntax")
(test/exn (parse 'if0) "Illegal syntax")
(test/exn (parse 'fun) "Illegal syntax")
(test (parse '(fun (a b) (* a b))) (fun '(a b) (binop * (id 'a) (id 'b))))
(test (parse '(fun (x) (+ x x))) (fun '(x) (binop + (id 'x) (id 'x))))
(test/exn (parse '(fun (x x) (+ x x))) "Duplicate identifiers")
(test/exn (parse '(with ([x 5] [x 7]) (+ 1 2))) "Duplicate identifiers")
(test (parse '(fun (x) x)) (fun '(x) (id 'x)))
(test (parse '(fun (x) (/ y x))) (fun '(x) (binop / (id 'y) (id 'x))))
(test (parse '(fun () x)) (fun '() (id 'x)))
