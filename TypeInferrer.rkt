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
        [(eq? 'rec-with (first se)) (rec-with
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

; (alpha-vary e) → Expr?
;  e : Expr?
; renames the identifiers using gensym
(define (alpha-vary e)
  (alpha-vary-rec e (mtEnv)))



(define (alpha-vary-rec e Env)
  (type-case Expr e
    [num (n) e]
    [bin-num-op (op lhs rhs)
                (bin-num-op op (alpha-vary-rec lhs Env) (alpha-vary-rec rhs Env))]
    [bool (b) (bool b)]
    [id (x) (if (eq? #f (lookup-env x Env))
                (error 'lookup "Unbound Identifier")
                (id (lookup-env x Env)))]
    [iszero (expr) (iszero (alpha-vary-rec expr Env))]
    [bif (c t f) (bif (alpha-vary-rec c Env) (alpha-vary-rec t Env) (alpha-vary-rec f Env))]
    [with (bound-id bound-body body) (local ([define newEnv (extend-env bound-id Env)])
                                            (with
                                              (lookup-env bound-id newEnv)
                                              (alpha-vary-rec bound-body Env)
                                              (alpha-vary-rec body newEnv)
                                            ))]
    [rec-with (bound-id bound-body body) (if (fun? bound-body)
                                           (local ([define newEnv (extend-env bound-id Env)])
                                              (rec-with
                                                 (lookup-env bound-id newEnv)
                                                 (alpha-vary-rec bound-body newEnv)
                                                 (alpha-vary-rec body newEnv)
                                            ))
                                           (local ([define newEnv (extend-env bound-id Env)])
                                              (rec-with
                                                 (lookup-env bound-id newEnv)
                                                 (alpha-vary-rec bound-body Env)
                                                 (alpha-vary-rec body newEnv)
                                            )))]
    [fun (arg-id body) (local ([define newEnv (extend-env arg-id Env)])
                         (fun
                          (lookup-env arg-id newEnv)
                          (alpha-vary-rec body newEnv)))]
    [app (fun-expr arg-expr) (app
                              (alpha-vary-rec fun-expr Env)
                              (alpha-vary-rec arg-expr Env))]
    [tempty () (tempty)]
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



;(generate-constraints e-id e) → (listof Constraint?)
;  e-id : symbol?
;  e : Expr?
;Returns the constraints generated by e. e-id serves as e’s label in this list.
(define (generate-constraints e-id e)
    (type-case Expr e
    [num (n) (list (eqc (t-var e-id) (t-num)))]
    [bin-num-op (op lhs rhs)
          (local ([define l (gensym 'binop-lhs)])
            (local([define r (gensym 'binop-rhs)])
             (append (list (eqc (t-var e-id) (t-num))
                           (eqc (t-var l) (t-num))
                           (eqc (t-var r) (t-num)))
                     (generate-constraints l lhs)
                     (generate-constraints r rhs))))]
    [bool (b) (list (eqc (t-var e-id) (t-bool)))]
    [id (x) (list (eqc (t-var e-id) (t-var x)))]
    [iszero (expr)
            (local ([define isZ (gensym 'isZeroBody)])
              (append
                (list
                   (eqc (t-var e-id) (t-bool))
                    (eqc (t-var isZ) (t-num)))
                (generate-constraints isZ expr)))]
    [bif (c t f)
          (local ([define co (gensym 'conditional)])
            (local([define tr (gensym 'condTrue)])
               (local([define fa (gensym 'condFalse)])
                  (append (list (eqc (t-var e-id) (t-var tr))
                                (eqc (t-var e-id) (t-var fa))
                                (eqc (t-var co) (t-bool)))
                          (generate-constraints co c)
                          (generate-constraints tr t)
                          (generate-constraints fa f)))))]
    [with (bound-id bound-body body)
         (local ([define bb (gensym 'withBoundBody)])
           (local
                ([define b  (gensym 'withBody)])
             (append
                  (list (eqc (t-var e-id) (t-var b))
                        (eqc (t-var bound-id) (t-var bb)))
                             (generate-constraints bb bound-body)
                             (generate-constraints b body))))]
    [rec-with (bound-id bound-body body)
         (local ([define bb (gensym 'withBoundBody)])
           (local ([define b  (gensym 'withBody)])
             (append
                  (list (eqc (t-var e-id) (t-var b))
                        (eqc (t-var bound-id) (t-var bb)))
                             (generate-constraints bb bound-body)
                             (generate-constraints b body))))]
    [fun (arg-id body)
         (local ([define bod (gensym 'funcBody)])
             (append
                  (list (eqc (t-var e-id) (t-fun (t-var arg-id) (t-var bod))))
                      (generate-constraints bod body)))]
    [app (fun-expr arg-expr)
         (local ([define func (gensym 'appfuncExpres)])
           (local ([define arg (gensym 'appArg)])
             (append
                  (list (eqc (t-var func) (t-fun (t-var arg) (t-var e-id))))
                      (generate-constraints func fun-expr)
                      (generate-constraints arg arg-expr))))]
    [tempty () (list (eqc (t-var e-id) (t-list (t-var (gensym 'listType)))))]
    [tcons (first rest)
        (local ([define f (gensym 'firstCons)])
          (local ([define r (gensym 'restCons)])
             (append
                  (list
                     (eqc (t-var e-id) (t-list (t-var f)))
                     (eqc (t-var r) (t-list (t-var f))))
                  (generate-constraints f first)
                  (generate-constraints r rest))))]
    [tfirst (expr)
      (local ([define ex (gensym 'firstList)])
        (local ([define f (gensym 'listType)])
        (append
           (list
              (eqc (t-var e-id) (t-var f))
              (eqc (t-var ex) (t-list (t-var f))))
                (generate-constraints ex expr))))]
    [trest (expr)
      (local ([define ex (gensym 'restList)])
        (local ([define f (gensym 'listType)])
        (append
           (list
              (eqc (t-var e-id) (t-list (t-var f)))
              (eqc (t-var ex) (t-list (t-var f))))
                (generate-constraints ex expr))))]      
    [istempty (expr)
      (local ([define ex (gensym 'empty?List)])
        (local ([define f (gensym 'listType)])
           (append
              (list
                (eqc (t-var e-id) (t-bool))
                (eqc (t-var ex) (t-list (t-var f))))
                   (generate-constraints ex expr))))]
    ))

(define (extend-subst-list subst list-of-subst)
  (cons subst list-of-subst))

(define (equal-identifiers constraint)
  (and
   (t-var? (eqc-lhs constraint))
   (t-var? (eqc-rhs constraint))
   (eq? (t-var-v (eqc-lhs constraint))
        (t-var-v (eqc-rhs constraint)))))

(define (subst-id id value lst return-lst)
  (if (not (empty? lst))
  (local ([define first-constraint (first lst)])
    (local ([define new-first-constraint
              (eqc
               (if (and (t-var? (eqc-lhs first-constraint)) (eq? (t-var-v (eqc-lhs first-constraint)) id))
                  value
                  (eqc-lhs first-constraint))
               (if (and (t-var? (eqc-rhs first-constraint)) (eq? (t-var-v (eqc-lhs first-constraint)) id))
                  value
                  (eqc-rhs first-constraint)))])
      
      (subst-id id value (rest lst) (cons new-first-constraint return-lst))
    ))
  return-lst))


(define (unify-recursive list-of-const list-of-substitutions)
  (if (not (empty? list-of-const))
      (local ([define first-constraint (first list-of-const)])
        (local ([define rest-of-const (rest list-of-const)])
          (cond
            ;1. If X and Y are identical identifiers, do nothing.
            [(equal-identifiers first-constraint) 
             (unify-recursive
              rest-of-const
              (extend-subst-list first-constraint list-of-substitutions))]
            [(t-var? (eqc-lhs first-constraint))
              (unify-recursive rest-of-const list-of-substitutions)]
            [(t-var? (eqc-rhs first-constraint))
             (unify-recursive rest-of-const list-of-substitutions)]
            [else (error 'unification "does not unify")]
            )
        )
      )
      ; else return list of substitutions
      list-of-substitutions))


;(unify loc) → (listof Constraint?)
; loc : (listof Constraint?)
; implements the unification algorithm on the list of constraints
(define (unify loc)
  (unify-recursive loc empty))

;(infer-type e) → Type?
;  e : Expr?
; infers the type of a variable on 
(define (infer-type e)
  "not implemented"
  )




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









;------------------------------------------------------------------------------
; Function: alpha-vary
; * Is the function correct?
; * Is the function documented correctly (i.e. contract and purpose statement)?
; * Is there an example of alpha-varying a number expression properly?
(test (alpha-vary (parse '6)) (num 6))
; * Is there an example of alpha-varying a true expression properly?
(test (alpha-vary (parse 'true)) (bool #t))
; * Is there an example of alpha-varying a false expression properly?
(test (alpha-vary (parse 'false)) (bool #f))
; * Is there an example of alpha-varying a + expression properly?
(test (alpha-vary (parse '(+ 1 2))) (bin-num-op + (num 1) (num 2)))
; * Is there an example of alpha-varying a - expression properly?
(test (alpha-vary (parse '(- 17 12))) (bin-num-op - (num 17) (num 12)))
; * Is there an example of alpha-varying a * expression properly?
(test (alpha-vary (parse '(* 3 4))) (bin-num-op * (num 3) (num 4)))
; * Is there an example of alpha-varying a iszero expression properly?
(test (alpha-vary (parse '(iszero 20))) (iszero (num 20)))
; * Is there an example of alpha-varying a bif expression properly?
(test (alpha-vary (parse '(bif true 1 2))) (bif (bool #t) (num 1) (num 2)))
; * Is there an example of alpha-varying a id expression properly?
(test/exn (alpha-vary (parse 'x))  "Unbound Identifier")
; * Is there an example of alpha-varying a with expression properly?
(alpha-vary (parse '(+ (with (x 4) x) (with (x 5) x))))
(alpha-vary (parse '(with (x 5) (with (x (+ x 6)) x))))
; * Is there an example of alpha-varying a rec expression properly?
(alpha-vary (parse '(rec-with (x 20) x))) (rec-with 'x (num 20) (id 'x))
; * Is there an example of alpha-varying a fun expression properly?
(alpha-vary (parse '(fun (x) x)))
(alpha-vary (parse '(fun (x) (with (x x) x))))
; * Is there an example of alpha-varying a app expression properly?
(alpha-vary (parse '((fun (x) x) 6)))
; * Is there an example of alpha-varying a tempty expression properly?
(test (alpha-vary (parse 'tempty)) (tempty))
; * Is there an example of alpha-varying a tcons expression properly?
(test (alpha-vary (parse '(tcons true tempty))) (tcons (bool #t) (tempty)))
; * Is there an example of alpha-varying a tempty? expression properly?
(test (alpha-vary (parse '(tempty? tempty))) (istempty (tempty)))
; * Is there an example of alpha-varying a tfirst expression properly?
(test (alpha-vary (parse '(tfirst (tcons true tempty)))) (tfirst (tcons (bool #t) (tempty))))
; * Is there an example of alpha-varying a trest expression properly?
(test (alpha-vary (parse '(trest (tcons true tempty)))) (trest (tcons (bool #t) (tempty))))


;Function: generate-constraints
; * Is the function correct?
; * Is the function documented correctly (i.e. contract and purpose statement)?
; * Is there an example of generating constraints for a number expression?
((constraint-list=? (generate-constraints (gensym) (alpha-vary (parse 6))))
                    (list (eqc (t-var (gensym)) (t-num))))
; * Is there an example of generating constraints for a true expression?
((constraint-list=? (generate-constraints (gensym) (alpha-vary (parse 'true))))
                    (list (eqc (t-var 'g23372) (t-bool))))
; * Is there an example of generating constraints for a false expression?
((constraint-list=? (generate-constraints (gensym) (alpha-vary (parse 'false))))
                    (list (eqc (t-var 'g23372) (t-bool))))
; * Is there an example of generating constraints for a + expression?
((constraint-list=? (generate-constraints (gensym) (alpha-vary (parse '(+ 1 2)))))
                    (list
                      (eqc (t-var 'g34286) (t-num))
                      (eqc (t-var 'binop-lhs34287) (t-num))
                      (eqc (t-var 'binop-rhs34288) (t-num))
                      (eqc (t-var 'binop-lhs34287) (t-num))
                      (eqc (t-var 'binop-rhs34288) (t-num))))
; * Is there an example of generating constraints for a - expression?
((constraint-list=? (generate-constraints (gensym) (alpha-vary (parse '(- 15 5)))))
                    (list
                      (eqc (t-var 'g34286) (t-num))
                      (eqc (t-var 'binop-lhs34287) (t-num))
                      (eqc (t-var 'binop-rhs34288) (t-num))
                      (eqc (t-var 'binop-lhs34287) (t-num))
                      (eqc (t-var 'binop-rhs34288) (t-num))))
; * Is there an example of generating constraints for a * expression?
((constraint-list=? (generate-constraints (gensym) (alpha-vary (parse '(* 3 4)))))
                    (list
                      (eqc (t-var 'g34286) (t-num))
                      (eqc (t-var 'binop-lhs34287) (t-num))
                      (eqc (t-var 'binop-rhs34288) (t-num))
                      (eqc (t-var 'binop-lhs34287) (t-num))
                      (eqc (t-var 'binop-rhs34288) (t-num))))
; * Is there an example of generating constraints for a iszero expression?
((constraint-list=? (generate-constraints (gensym) (alpha-vary (parse '(iszero 6)))))
                   (list
                    (eqc (t-var 'g51412) (t-bool))
                    (eqc (t-var 'isZeroBody51413) (t-num))
                    (eqc (t-var 'isZeroBody51413) (t-num))))
; * Is there an example of generating constraints for a bif expression?
((constraint-list=? (generate-constraints (gensym) (alpha-vary (parse '(bif true 13 14)))))
                   (list
                    (eqc (t-var 'g56935) (t-var 'condTrue56937))
                    (eqc (t-var 'g56935) (t-var 'condFalse56938))
                    (eqc (t-var 'conditional56936) (t-bool))
                    (eqc (t-var 'conditional56936) (t-bool))
                    (eqc (t-var 'condTrue56937) (t-num))
                    (eqc (t-var 'condFalse56938) (t-num))))
; * Is there an example of generating constraints for a id expression?
((constraint-list=? (generate-constraints (gensym) (alpha-vary (parse '(with  [x 5] x)))))
                   (list
                     (eqc (t-var 'g110324) (t-var 'withBody110327))
                     (eqc (t-var 'x110325) (t-var 'withBoundBody110326))
                     (eqc (t-var 'withBoundBody110326) (t-num))
                     (eqc (t-var 'withBody110327) (t-var 'x110325))))
; * Is there an example of generating constraints for a with expression?
((constraint-list=? (generate-constraints (gensym) (alpha-vary (parse '(with  [x 5] (+ x 5))))))
                   (list
                    (eqc (t-var 'g164392) (t-var 'withBody164395))
                    (eqc (t-var 'x164393) (t-var 'withBoundBody164394))
                    (eqc (t-var 'withBoundBody164394) (t-num))
                    (eqc (t-var 'withBody164395) (t-num))
                    (eqc (t-var 'binop-lhs164396) (t-num))
                    (eqc (t-var 'binop-rhs164397) (t-num))
                    (eqc (t-var 'binop-lhs164396) (t-var 'x164393))
                    (eqc (t-var 'binop-rhs164397) (t-num))))
; * Is there an example of generating constraints for a rec expression?
((constraint-list=? (generate-constraints (gensym) (alpha-vary (parse '(rec-with [f (fun (x) (f 1))] (f 2))))))
                  (list
                   (eqc (t-var 'g198450) (t-var 'withBody198454))
                   (eqc (t-var 'f198451) (t-var 'withBoundBody198453))
                   (eqc (t-var 'withBoundBody198453) (t-fun (t-var 'x198452) (t-var 'funcBody198455)))
                   (eqc (t-var 'appfuncExpres198456) (t-fun (t-var 'appArg198457) (t-var 'funcBody198455)))
                   (eqc (t-var 'appfuncExpres198456) (t-var 'f198451))
                   (eqc (t-var 'appArg198457) (t-num))
                   (eqc (t-var 'appfuncExpres198458) (t-fun (t-var 'appArg198459) (t-var 'withBody198454)))
                   (eqc (t-var 'appfuncExpres198458) (t-var 'f198451))
                   (eqc (t-var 'appArg198459) (t-num))))
; * Is there an example of generating constraints for a fun expression?
((constraint-list=? (generate-constraints (gensym) (alpha-vary (parse '(fun (x) (+ x 2))))))
                  (list
                   (eqc (t-var 'g221456) (t-fun (t-var 'x221457) (t-var 'funcBody221458)))
                   (eqc (t-var 'funcBody221458) (t-num))
                   (eqc (t-var 'binop-lhs221459) (t-num))
                   (eqc (t-var 'binop-rhs221460) (t-num))
                   (eqc (t-var 'binop-lhs221459) (t-var 'x221457))
                   (eqc (t-var 'binop-rhs221460) (t-num))))
; * Is there an example of generating constraints for a app expression?
((constraint-list=? (generate-constraints (gensym) (alpha-vary (parse '( (fun (x) (+ x 2)) 7)))))
                  (list
                   (eqc (t-var 'appfuncExpres238607) (t-fun (t-var 'appArg238608) (t-var 'g238605)))
                   (eqc (t-var 'appfuncExpres238607) (t-fun (t-var 'x238606) (t-var 'funcBody238609)))
                   (eqc (t-var 'funcBody238609) (t-num))
                   (eqc (t-var 'binop-lhs238610) (t-num))
                   (eqc (t-var 'binop-rhs238611) (t-num))
                   (eqc (t-var 'binop-lhs238610) (t-var 'x238606))
                   (eqc (t-var 'binop-rhs238611) (t-num))
                   (eqc (t-var 'appArg238608) (t-num))))
; * Is there an example of generating constraints for a tempty expression?
((constraint-list=? (generate-constraints (gensym) (alpha-vary (parse 'tempty))))
                    (list (eqc (t-var 'g63109) (t-list (t-var 'listType63110)))))
; * Is there an example of generating constraints for a tcons expression?
((constraint-list=? (generate-constraints (gensym) (alpha-vary (parse '(tcons true tempty)))))
                    (list
                     (eqc (t-var 'g69141) (t-list (t-var 'firstCons69142)))
                     (eqc (t-var 'restCons69143) (t-list (t-var 'firstCons69142)))
                     (eqc (t-var 'firstCons69142) (t-bool))
                     (eqc (t-var 'restCons69143) (t-list (t-var 'listType69144))))) 
; * Is there an example of generating constraints for a tfirst expression?
((constraint-list=? (generate-constraints (gensym) (alpha-vary (parse '(tfirst (tcons true tempty))))))
                    (list
                      (eqc (t-var 'g80470) (t-var 'listType80472))
                      (eqc (t-var 'firstList80471) (t-list (t-var 'listType80472)))
                      (eqc (t-var 'firstList80471) (t-list (t-var 'firstCons80473)))
                      (eqc (t-var 'restCons80474) (t-list (t-var 'firstCons80473)))
                      (eqc (t-var 'firstCons80473) (t-bool))
                      (eqc (t-var 'restCons80474) (t-list (t-var 'listType80475)))))

; * Is there an example of generating constraints for a trest expression?
((constraint-list=? (generate-constraints (gensym) (alpha-vary (parse '(trest (tcons true tempty))))))
                    (list
                      (eqc (t-var 'g87084) (t-list (t-var 'listType87086)))
                      (eqc (t-var 'restList87085) (t-list (t-var 'listType87086)))
                      (eqc (t-var 'restList87085) (t-list (t-var 'firstCons87087)))
                      (eqc (t-var 'restCons87088) (t-list (t-var 'firstCons87087)))
                      (eqc (t-var 'firstCons87087) (t-bool))
                      (eqc (t-var 'restCons87088) (t-list (t-var 'listType87089)))))









