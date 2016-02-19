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
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(list? sexp)
     (cond
       [ (empty? sexp) (error 'parse "Illegal syntax")]
       [ (eqv? with (lookup-op (first sexp)))
         (if (is-valid-with? sexp)
             (with
              (check-multiple-bindings
               (map
                (lambda (x)
                  (binding (first x) (parse (second x)))) (second sexp)))
              (parse (third sexp)))
             (error 'parse "Illegal syntax"))]
       [(eq? (first sexp) 'if0)
        (if (is-valid-if0? sexp)
            (if0 (parse (second sexp)) (parse (third sexp)) (parse (fourth sexp)))
            (error 'parse "Illegal syntax"))]
       [(is-valid-binop? sexp)
        (binop (lookup-op (first sexp)) (parse (second sexp)) (parse (third sexp)))]
       [(eq? (first sexp) 'fun)
        (if (is-valid-fun? sexp)
            (fun
             (duplicate-ids-in-fun? (second sexp))
             (parse (third sexp)))
            (error 'parse "Illegal syntax"))]
       [else  (app
               (parse (first sexp))
               (foldr
                (lambda (a b)
                  (cons (parse a) b))
                empty
                (rest sexp)))]
       )]
     [(is-valid-identifier? sexp)
      (id sexp)]
    [else (error 'parse "Illegal syntax")]))

(define (eval-binop op l r)
  (numV (op (numV-n l) (numV-n r))))




(define (check-division-by-zero exp)
  (if (and (num? (binop-rhs exp))
           (= 0 (num-n (binop-rhs exp)))
           (eqv? / (binop-op exp)))
      #t
      #f))

(define (check-non-numeric-value exp)
  (if (not (numV? exp))
      (error 'check-non-numeric-value "Non-Numeric Value in if statement")
      exp))


;; lookup : symbol Env -> FWAE-Value
;; looks up an identifier in an environment and returns the value
;; bound to it (or reports error if not found)
(define (lookup name env)
  (type-case Env env
    [mtEnv () (error 'lookup "Unbound Identifier")]
    [anEnv (bound-name bound-value rest-env)
           (if (symbol=? bound-name name)
               bound-value
               (lookup name rest-env))]))


; interp : CFWAE Env -> CFWAE-Value
; This procedure interprets the given CFWAE in the environment
; and produces a result in the form of a CFWAE-Value
(define (interp expr env)
  (type-case CFWAE expr
    [num (n) (numV n)]
    [binop (op l r)
           (if (check-division-by-zero (binop op
                                               (num (numV-n (interp l env)))
                                               (num (numV-n (interp r env)))))
               (error 'interp "Division by zero")
               (eval-binop op (interp l env) (interp r env)))]
    [id (v) (lookup v env)]
    [if0 (c t e)
         (if (zero? (numV-n
                     (check-non-numeric-value
                      (interp c env))))
             (interp t env)
             (interp e env))]
    [else (error 'interp "unimplemented")]
    ))

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
; extra parse tests

; legal syntax

(test (parse '(+ 1 (+ 1 2))) (binop + (num 1) (binop + (num 1) (num 2))))
(test (parse '(/ 10 (/ 100 10))) (binop / (num 10) (binop / (num 100) (num 10))))
(test (parse '(* 3 4)) (binop * (num 3) (num 4)))
(test (parse '(with ([x 2]) (with ([x 1]) x))) (with (list (binding 'x (num 2))) (with (list (binding 'x (num 1))) (id 'x))))
(test (parse '(2 3)) (app (num 2) (list (num 3))))
(test (parse '(/ 10 (100 10))) (binop / (num 10) (app (num 100) (list (num 10)))))
(test (parse '(with ([x 5] [y 6] [z 7]) (+ 1 2))) (with (list (binding 'x (num 5)) (binding 'y (num 6)) (binding 'z (num 7))) (binop + (num 1) (num 2))))


; illegal syntax
(test/exn (parse '(with ([x 1] [x 2]) (+ x x))) "Duplicate identifiers")
(test/exn (parse true) "Illegal syntax")
(test/exn (parse '(+ 3 +)) "Illegal syntax")
(test/exn (parse '(4 3 +)) "Illegal syntax")
(test/exn (parse '(4 + 3 + 5)) "Illegal syntax")
(test/exn (parse '(5 + 6)) "Illegal syntax")
(test/exn (parse '(with (+ 1 2))) "Illegal syntax")
(test/exn (parse '(with [x 5] [y 6] (+ 1 2))) "Illegal syntax")
(test/exn (parse '(with x (+ 1 2))) "Illegal syntax")
(test/exn (parse '(with  (+ 1 2) ([x 1]))) "Illegal syntax")
(test/exn (parse '(with x x)) "Illegal syntax")
(test/exn (parse '(with () (+ 1 2))) "Illegal syntax")
(test/exn (parse '(with (x) (+ 1 2))) "Illegal syntax")
(test/exn (parse '(with ([+ 1]) (+ 1 2))) "Illegal syntax")
(test/exn (parse '(with [x 1] (+ 1 2))) "Illegal syntax")
(test/exn (parse "Test") "Illegal syntax")
(test/exn (parse '(with ((x 5 6)) (+ 1 x))) "Illegal syntax")
(test/exn (parse '(with ((42 6)) (+ 1 x))) "Illegal syntax")


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





; Function: calc
; a number case test?
;  a + case test?
; a - case test?
; a * case test?
; a / case test?
; a divide by zero case test?
(test/exn (interp (parse '(/ 6 0)) (mtEnv)) "Division by zero")
(test/exn (interp (parse '(/ (+ 6 0) 0)) (mtEnv)) "Division by zero")
(test/exn (interp (parse '(/ (/ 6 0) 1)) (mtEnv)) "Division by zero")


; interp test cases
(test (interp (parse '(+ 1 2)) (mtEnv)) (numV 3))
(test (interp (parse '(+ 1 (/ 100 10))) (mtEnv)) (numV 11))
(test (interp (parse '(+ (* 4 (+ 1 2)) (/ 100 10))) (mtEnv)) (numV 22))
(test (interp (parse '(+ (* (- 6 1) (+ 1 2)) (/ 100 10))) (mtEnv)) (numV 25))
(test (interp (parse '5) (mtEnv)) (numV 5))
(test/exn (interp (parse '(+ x x)) (mtEnv)) "Unbound Identifier")
(test/exn (interp (parse 'x) (mtEnv)) "Unbound Identifier")





; if tests

(test (interp (parse '(if0 0 1 2)) (mtEnv)) (numV 1))
(test (interp (parse '(if0 7 1 2)) (mtEnv)) (numV 2))
(test (interp (parse '(if0 (- 1 1) (+ 3 4) 0)) (mtEnv)) (numV 7))
(test (interp (parse '(if0 (+ 1 1) 12 (/ 100 10))) (mtEnv)) (numV 10))