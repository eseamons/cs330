#lang plai


(define-type Expr
  [num (n number?)]
  [id (v symbol?)]
  [bool (b boolean?)]
  [bin-num-op (op procedure?) (lhs Expr?) (rhs Expr?)]
  [iszero (e Expr?)]
  [bif (test Expr?) (then Expr?) (else Expr?)]
  [with (bound-id symbol?) (bound-body Expr?) (body Expr?)]
  [rec-with (bound-id symbol?) (bound-body Expr?) (body Expr?)]
  [fun (arg-id symbol?) (body Expr?)]
  [app (fun-expr Expr?) (arg-expr Expr?)]
  [tempty]
  [tcons (first Expr?) (rest Expr?)]
  [tfirst (e Expr?)]
  [trest (e Expr?)]
  [istempty (e Expr?)])
 
(define-type Type
  [t-num]
  [t-bool]
  [t-list (elem Type?)]
  [t-fun (arg Type?) (result Type?)]
  [t-var (v symbol?)])
 
(define-type Constraint
  [eqc (lhs Type?) (rhs Type?)])

(define-type Env
  [mtEnv]
  [anEnv (name symbol?) (genName symbol?) (env Env?)])


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
; parses concrete syntax to abstract syntax
(define (parse se)
  (cond
    [(list? se)
     (cond
        [(eq? 'with (first se)) (with
                  (first (second se))
                  (parse (second (second se)))
                  (parse (third se)))]
        [(eq? 'rec (first se)) (rec-with
                  (first (second se))
                  (parse (second (second se)))
                  (parse (third se)))]
         [(not (eq? #f (lookup-op (first se))))
          (bin-num-op (lookup-op (first se)) (parse (second se)) (parse (third se)))]

         [(eq? 'iszero (first se)) (iszero (parse (second se)))]
         [(eq? 'bif (first se)) (bif (parse (second se)) (parse (third se)) (parse (fourth se)))]
         [(eq? 'tcons (first se)) (tcons (parse (second se)) (parse (third se)))]
         [(eq? 'tempty? (first se)) (istempty (parse (second se)))]
         [(eq? 'tfirst (first se)) (tfirst (parse (second se)))]
         [(eq? 'trest (first se)) (trest (parse (second se)))]
         [(eq? 'fun (first se)) (fun
                                   (first (second se))
                                   (parse (third se)))]
         [else (app (parse (first se)) (parse (second se)))]
         )]
    [(number? se) (num se)]
    [(eq? 'true se) (bool #t)]
    [(eq? 'false se) (bool #f)]
    [(eq? 'tempty se) (tempty)]
    [(symbol? se) (id se)]
    ))

(define (alpha-vary e)
  "not implemented")

(define (generate-constraints e-id e)
  "not implemented")

(define (unify loc)
  "not implemented")

(define (infer-type e)
  "not implemented")
