#lang plai


; Assignment: Rudimentary Interpreter
; <Eric Seamons>

(define-type Binding
  [binding (name symbol?) (named-expr WAE?)])
 
(define-type WAE
  [num (n number?)]
  [binop (op procedure?)
         (lhs WAE?)
         (rhs WAE?)]
  [with (lob (listof Binding?)) (body WAE?)]
  [id (name symbol?)])

(define op-table
  (list (list '+ +) (list '- -) (list '* *) (list '/ /) (list 'with with)))


; lookup-op : (op) -> (or/c procedure? false/c)
; a function that extracts the definition of an operator or false
(define (lookup-op op)
  (if (eqv? #f (assoc op op-table))
      #f
  (second (assoc op op-table))))

;; helper functions for parser

(define (is-valid-identifier? x)
  (and (symbol? x) (eq? #f (lookup-op x))))

(define (is-valid-binop? x)
  (and (= 3 (length x)) (procedure? (lookup-op (first x)))))

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



;; parse : (s-exp) -> WAE
;; Parses s-exp into a WAE
(define (parse s-exp)
  (cond
    [(number? s-exp) (num s-exp)]
    [(list? s-exp)
     (cond
       [ (empty? s-exp) (error 'parse "Illegal syntax")]
       [ (eqv? with (lookup-op (first s-exp)))
         (if (is-valid-with? s-exp)
             (with
              (map
               (lambda (x)
                 (binding (first x) (parse (second x)))) (second s-exp))
              (parse (third s-exp)))
             (error 'parse "Illegal syntax"))]
       [(is-valid-binop? s-exp)
        (binop (lookup-op (first s-exp)) (parse (second s-exp)) (parse (third s-exp)))]
       [else  (error 'parse "Illegal syntax")]
       )]
     [(is-valid-identifier? s-exp)
      (id s-exp)]
    [else (error 'parse "Illegal syntax")]))


(define (is-in-binding-list? bind lst)
  (ormap
   (lambda (x)
     (symbol=? (binding-name x) (binding-name bind)))
   lst))

(define (check-multiple-bindings bindings)
  (foldr
   (lambda (a b)
           (if (is-in-binding-list? a b)
               (error "Multiple Bindings")
               (cons a b)
                ))
   empty
   bindings))

(define (subst-bindings inner-bindings outer-bindings)
  (foldr
   (lambda (a b)
     (cons (binding (binding-name a) (subst* outer-bindings (binding-named-expr a))) b))
   empty
   inner-bindings))
   



(define (subst* lob body)
  (type-case WAE body
    [num (n) (num n)]
    [id (v) (foldl
             (lambda (a b)
               (if (and (symbol? b) (symbol=? (binding-name a) b))
                   (binding-named-expr a)
                   b
               ))
             v
             lob)]
    [binop (op l r) (binop op
                     (if (symbol? (subst* lob l))
                         (id (subst* lob l))
                         (subst* lob l))
                     (if (symbol? (subst* lob r))
                         (id (subst* lob r))
                         (subst* lob r)))]
    [with (lb body) (subst* (append (subst-bindings (check-multiple-bindings lb) lob) lob) body)]))


(define (check-division-by-zero? exp)
  (if (and (num? (binop-rhs exp))
           (= 0 (num-n (binop-rhs exp)))
           (eqv? / (binop-op exp)))
      #t
      #f))

; calc: (e) → number?
; Consumes a WAE representation of an expression and computes the corresponding numerical result, eagerly
(define (calc expr)
  (type-case WAE expr
    [num (n) n]
    [binop (op l r)
           (if (check-division-by-zero? (binop op (num (calc l)) (num (calc r))))
               (error 'calc "Division by zero")
               (op (calc l) (calc r)))
               ]
    [with (lob body)
          (calc
           (if (symbol? (subst* (check-multiple-bindings lob) body))
               (id (subst* lob body))
               (subst* lob body)
                ))]
    [id (v) (error 'calc "Unbound Identifier")]))





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



; Function: calc
; a number case test?
;  a + case test?
; a - case test?
; a * case test?
; a / case test?
; a divide by zero case test?
(test/exn (calc (parse '(/ 6 0))) "Division by zero")
(test/exn (calc (parse '(/ (+ 6 0) 0))) "Division by zero")
(test/exn (calc (parse '(/ (/ 6 0) 1))) "Division by zero")
; a case that shows referencing an identifier
(test (calc (parse '(with ((x 26)) x))) 26)
; an id (unbound) case test
(test/exn (calc (parse '(with ([y 11]) x))) "Unbound Identifier")
; a with (basic, bound id) case test
(test (calc (parse '(with ([x 3]) (* x x)))) 9)
; a with (shadowing) case test
(test (calc (parse '(with ([x 3]) (with ((x 4)) (+ x 20) )))) 24)
; a with (shadowing in body but not in initialization expression) case test 
(test (calc (parse '(with ([y 7] [x 4]) (with ([x (+ x 1)]) (+ y x))))) 12)
; Is there a duplicate bindings test case
(test/exn (calc (parse '(with ([x 1] [x 2]) (+ 1 x)))) "Multiple Bindings")





; legal syntax

(test (parse '(+ 1 (+ 1 2))) (binop + (num 1) (binop + (num 1) (num 2))))
(test (parse '(/ 10 (/ 100 10))) (binop / (num 10) (binop / (num 100) (num 10))))
(test (parse '(* 3 4)) (binop * (num 3) (num 4)))
(test (parse '(with ([x 1] [x 2]) (+ x x))) (with (list (binding 'x (num 1)) (binding 'x (num 2))) (binop + (id 'x) (id 'x))))
(test (parse '(with ([x 1] [x 2]) (+ x x))) (with (list (binding 'x (num 1)) (binding 'x (num 2))) (binop + (id 'x) (id 'x))))
;(parse '(with ([x 1]) (with ([x 1]) x)))
(test (parse '(with ([x 5] [y 6] [z 7]) (+ 1 2))) (with (list (binding 'x (num 5)) (binding 'y (num 6)) (binding 'z (num 7))) (binop + (num 1) (num 2))))
(test (parse '(with ([x 1] [x 2]) (+ x x)))
      (with (list (binding 'x (num 1))
                  (binding 'x (num 2)))
            (binop + (id 'x) (id 'x))))

; illegal syntax
(test/exn (parse '(2 3)) "Illegal syntax")
(test/exn (parse '(/ 10 (100 10))) "Illegal syntax")
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

(test (calc (parse '(+ 1 2))) 3)
(test (calc (parse '(+ 1 (/ 100 10)))) 11)
(test (calc (parse '(+ (* 4 (+ 1 2)) (/ 100 10)))) 22)
(test (calc (parse '(+ (* (- 6 1) (+ 1 2)) (/ 100 10)))) 25)
(test (calc (parse '5)) 5)
(test (calc (parse '(with ([x 5]) x))) 5)
(test (calc (parse '(with ([x 5]) (+ x x)))) 10)
(test (calc (parse '(with ([y 11]) y))) 11)
(test (calc (parse '(with ([x 5] [y 11]) y))) 11)
(test (calc (parse '(with ([x 5] [y 10] [z 30]) 13))) 13)
(test (calc (parse '(with ([x 1] [y 3] [z 4]) (+ x (* y z))))) 13)
(test (calc (parse '(with ([w 2] [x 1] [y 3] [z 4]) (+ (* x z) (* w y))))) 10)
(test (calc (parse '(with ([w 2] [x 1] [y 3] [z 4]) (* (* x z) (* w y))))) 24)
(test (calc (parse '(with ([x 2] [y 4] [z 6] [a 7] [b 20]) (+ a b)))) 27)
(test (calc (parse '(with ([x 2] [y 4] [z 6] [a 7] [b 20]) (+ x (+ a b))))) 29)

(test/exn (calc (parse '(+ x x))) "Unbound Identifier")
(test/exn (calc (parse 'x)) "Unbound Identifier")
(test/exn (calc (parse '(with ([y x]) y))) "Unbound Identifier")
(test/exn (calc (parse '(with ([y 11]) (+ x x)))) "Unbound Identifier")
(test/exn (calc (parse '(with ([x 5] [y 10] [z 30]) a))) "Unbound Identifier")
(test/exn (calc (parse '(with ([x 5] [y 10] [z 30]) (+ a 1)))) "Unbound Identifier")

(test (calc (parse '(with ([x 1]) (with ([x 4] [z (+ x 2)]) (+ x z))))) 7)
(test/exn (calc (parse '(with ([x 1]) (with ([x 4] [x (+ x 2)]) (+ x z))))) "Multiple Bindings")

s

(test (calc (parse '(with ([y 1] [x 2]) (with ([x 1]) (+ x 1))))) 2)

;(list (binding 'x (num 24)) (binding 'y (num 24)) (binding 'z (num 24)))

(define lob (list (binding 'x (num 27)) (binding 'y (num 28)) (binding 'z (num 29))))
(define lob2 (list (binding 'x (num 24)) (binding 'y (num 24))))
(define lob3 (list (binding 'x (binop + (id 'y) (id 'z))) (binding 'y (binop + (id 'x) (id 'x)))))
(define one-bind (binding 'a (num 24)))


;(subst-bindings lob3 lob)

(test (subst* (list (binding 'x (num 1))) (id 'x))
      (num 1))
(test (subst* (list (binding 'x (num 1))
                    (binding 'y (num 2)))
              (binop + (id 'x) (id 'y)))
      (binop + (num 1) (num 2)))

