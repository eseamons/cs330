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



(define (extend-env id Env)
  (anEnv id
         (gensym id)
         Env))


(define (lookup-env name env)
  (type-case Env env
    [mtEnv () #f]
    [anEnv (bound-name bound-genName rest-env)
           (if (symbol=? bound-name name)
               bound-genName
               (lookup-env name rest-env))]))




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
  (alpha-vary-rec e (mtEnv)))

(define (alpha-vary-rec e Env)
  (type-case Expr e
    [num (n) e]
    [bin-num-op (op lhs rhs)
                (bin-num-op op (alpha-vary-rec e Env) (alpha-vary-rec e Env))]
    [bool (b) (bool b)]
    [id (x) (if (eq? #f (lookup-env x Env))
                (error 'lookup "Unbound Identifier")
                (lookup-env x Env))]
    [iszero (expr) (iszero (alpha-vary-rec expr Env))]
    [bif (c t f) (bif (alpha-vary-rec c Env) (alpha-vary-rec t Env) (alpha-vary-rec f Env))]
    [with (bound-id bound-body body) (with
                                      (alpha-vary-rec bound-id Env)
                                      (alpha-vary-rec bound-body Env)
                                      (alpha-vary-rec body Env))]
    [rec-with (bound-id bound-body body) (rec-with
                                          (alpha-vary-rec bound-id Env)
                                          (alpha-vary-rec bound-body Env)
                                          (alpha-vary-rec body Env))]
    [fun (arg-id body) (fun
                        (alpha-vary-rec arg-id Env)
                        (alpha-vary-rec body Env))]
    [app (fun-expr arg-expr) (app
                              (alpha-vary-rec fun-expr Env)
                              (alpha-vary-rec arg-expr Env))]
    [tempty () tempty]
    [tcons (first rest) (tcons
                         (alpha-vary-rec first Env)
                         (alpha-vary-rec rest Env))]
    [tfirst (expr) (tfirst
                    (alpha-vary-rec expr Env))]
    [trest (expr) (trest
                      (alpha-vary-rec expr Env))]
    [istempty (expr) (istempty
                      (alpha-vary-rec expr Env))]
    ))

(define (generate-constraints e-id e)
  "not implemented")

(define (unify loc)
  "not implemented")

(define (infer-type e)
  "not implemented")




; type=?/mapping : hash hash Type Type -> Bool
; determines if types are equal modulo renaming
(define (type=?/mapping ht1 ht2 t1 t2)
  (define (teq? t1 t2)
    (type=?/mapping ht1 ht2 t1 t2))
  (cond
    [(and (t-num? t1) (t-num? t2)) true]
    [(and (t-bool? t1) (t-bool? t2)) true]
    [(and (t-list? t1) (t-list? t2))
     (teq? (t-list-elem t1) (t-list-elem t2))]
    [(and (t-fun? t1) (t-fun? t2))
     (and (teq? (t-fun-arg t1) (t-fun-arg t2))
          (teq? (t-fun-result t1) (t-fun-result t2)))]
    [(and (t-var? t1) (t-var? t2))
     (local ([define v1 ; the symbol that ht1 says that t1 maps to
               (hash-ref
                ht1 (t-var-v t1)
                (lambda ()
                  ; if t1 doesn't map to anything, it's the first
                  ; time we're seeing it, so map it to t2
                  (hash-set! ht1 (t-var-v t1) (t-var-v t2))
                  (t-var-v t2)))]
             [define v2
               (hash-ref
                ht2 (t-var-v t2)
                (lambda ()
                  (hash-set! ht2 (t-var-v t2) (t-var-v t1))
                  (t-var-v t1)))])
       ; we have to check both mappings, so that distinct variables
       ; are kept distinct. i.e. a -> b should not be isomorphic to
       ; c -> c under the one-way mapping a => c, b => c.
       (and (symbol=? (t-var-v t2) v1)
            (symbol=? (t-var-v t1) v2)))]
    [(and (Type? t1) (Type? t2)) false]
    [else (error 'type=? "either ~a or ~a is not a Type" t1 t2)]))
 
; type=? Type -> Type -> Bool
; signals an error if arguments are not variants of Type
(define ((type=? t1) t2)
  (or (type=?/mapping (make-hash) (make-hash) t1 t2)
      ; Unfortunately, test/pred simply prints false;
      ; this helps us see what t2 was.
      (error 'type=?
             "~s and ~a are not equal (modulo renaming)"
             t1 t2)))
 #|
(test/pred (t-var 'a)
           (type=? (t-var 'b)))
(test/pred (t-fun (t-var 'a) (t-var 'b))
           (type=? (t-fun (t-var (gensym)) (t-var (gensym)))))
(test/pred (t-fun (t-var 'a) (t-var 'b))
           (type=? (t-fun (t-var (gensym)) (t-var (gensym)))))
(test/pred (t-fun (t-var 'a) (t-var 'a)) ; fails
           (type=? (t-fun (t-var (gensym)) (t-var (gensym)))))
(test/pred (t-fun (t-var 'a) (t-var 'b)) ; fails
           (type=? (t-fun (t-var 'c) (t-var 'c))))
(test/exn ((type=? 34) 34) "not a Type")
|#
 
; constraint-list=? : Constraint list -> Constraint list -> Bool
; signals an error if arguments are not variants of Constraint
(define ((constraint-list=? lc1) lc2)
  (define htlc1 (make-hash))
  (define htlc2 (make-hash))
  (or (andmap (lambda (c1 c2)
                (and
                 (type=?/mapping
                  htlc1 htlc2
                  (eqc-lhs c1) (eqc-lhs c2))
                 (type=?/mapping
                  htlc1 htlc2
                  (eqc-rhs c1) (eqc-rhs c2))))
              lc1 lc2)
      (error 'constraint-list=?
             "~s and ~a are not equal (modulo renaming)"
             lc1 lc2)))


