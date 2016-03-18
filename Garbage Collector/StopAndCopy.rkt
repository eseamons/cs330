#lang plai/collector

(define heap-ptr 'null)
(define from-end 0)
(define to-end 0)
(define curr-space 'null)
(define prev-root 'null)
(define is-child-of-root #f)

(define (init-allocator)
  (begin
    (set! curr-space 'from-space)
    (set! heap-ptr 0)
    (set! from-end (- (/ (heap-size) 2) 1))
    (set! to-end (heap-size))))

(define (gc:alloc-flat p)
  (begin
    (if (eq? curr-space 'from-space)
        (when (> (+ heap-ptr 2) from-end)
          (begin
            (swap-space)
            (when (procedure? p)
              (copy (procedure-roots p)))
            (copy (get-root-set))))
        (when (> (+ heap-ptr 2) to-end)
          (begin
            (swap-space)
            (when (procedure? p)
              (copy (procedure-roots p)))
            (copy (get-root-set)))))
    (heap-set! heap-ptr 'prim)
    (heap-set! (+ 1 heap-ptr) p)
    (set! heap-ptr (+ 2 heap-ptr))
    (- heap-ptr 2)))

(define (gc:cons f r)
  (begin
    (if (eq? curr-space 'from-space)
        (when (> (+ heap-ptr 2) from-end)
          (begin
            (swap-space)
            (copy (get-root-set f r))))
        (when (> (+ heap-ptr 2) to-end)
          (begin
            (swap-space)
            (copy (get-root-set f r)))))
    (heap-set! heap-ptr 'cons)
    (heap-set! (+ 1 heap-ptr) f)
    (heap-set! (+ 2 heap-ptr) r)
    (set! heap-ptr (+ 3 heap-ptr))
    (- heap-ptr 3)))

(define (gc:cons? a)
  (eq? (heap-ref a) 'cons))

(define (gc:first a)
  (heap-ref (+ 1 a)))

(define (gc:rest a)
  (heap-ref (+ 2 a)))

(define (gc:set-first! a f)
  (if (gc:cons? a)
      (heap-set! (+ 1 a) f)
      (error 'set-first! "expects address of cons")))

(define (gc:set-rest! a r)
  (if (gc:cons? a)
      (heap-set! (+ 2 a) r)
      (error 'set-rest! "expects address of cons")))

(define (gc:flat? a)
  (eq? (heap-ref a) 'prim))

(define (gc:deref a)
  (heap-ref (+ 1 a)))


; custom functions

;(copy rs) → void?
;  rs : (listof root?)
;Goes through the current heap and copies everything that is reachable from the root set
(define (copy rs)
  (map (lambda (root) (begin
                        (set! is-child-of-root #t)
                        (copy-cell (read-root root) root))) rs))

;(copy-cell cell root) → void?
;  cell : location?
;  root : root?
;Updates roots and copies cells to the other heap
(define (copy-cell cell root)
  (begin
    (when (boolean=? is-child-of-root #t)
      (set-root! root heap-ptr)
      (set! is-child-of-root #f))
    (cond
      [(gc:flat? cell)
       (begin
         (heap-set! heap-ptr (heap-ref cell))
         (set! heap-ptr (+ 1 heap-ptr))
         (heap-set! heap-ptr (heap-ref (+ 1 cell)))
         (set! heap-ptr (+ 1 heap-ptr))
         )]
      [(procedure? cell)
       (copy (procedure-roots cell))]
      [(gc:cons? cell)
       (begin
         (copy-cell (heap-ref (+ 1 cell)) root)
         (copy-cell (heap-ref (+ 2 cell)) root))])))

;(swap-space) → void?
;Once the current space is full, it swaps to the other space and set heap-ptr to the beginning
;of that space
(define (swap-space)
  (if (eq? curr-space 'from-space)
      (begin
        (set! heap-ptr (/ (heap-size) 2))
        (set! curr-space 'to-space))
      (begin
        (set! heap-ptr 0)
        (set! curr-space 'from-space))))