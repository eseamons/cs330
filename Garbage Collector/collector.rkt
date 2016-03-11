#lang plai/collector

(define heap-ptr 'uninitialized-heap-ptr)
(define free-ptr 'uninitialized-free-ptr) ;points to first item in free list
(define prev-free-ptr 'uninitialized-prev-free-ptr)
(define next-free-ptr 'uninitialized-next-free-ptr)

(define (init-allocator)
  (begin
    (set! free-ptr 0)
    (set! heap-ptr 0)
    (create-free-list)
    (set! heap-ptr 0)
    ))


(define (create-free-list)
  (begin
    (heap-set! heap-ptr 'free)
    (heap-set! (+ 3 heap-ptr) 'empty)
    (when (not (eq? prev-free-ptr 'uninitialized-prev-free-ptr))
      (heap-set! (+ 3 prev-free-ptr) heap-ptr)
     )
    (set! prev-free-ptr heap-ptr)
    (set! heap-ptr (+ 4 heap-ptr))
    (when (not (> (+ heap-ptr 4) (heap-size)))
      (create-free-list)
      )
   ))

(define (gc:alloc-flat p)
  (begin
    (error 'test "test")
    (when (> (+ heap-ptr 4) (heap-size))
      #|
      (begin
         ; got here
        (when (procedure? p)
          (mark-and-sweep (procedure-roots p)))
        (mark-and-sweep (get-root-set))
        (set! heap-ptr 0)
        (free-heap heap-ptr)
        )
|#
(error 'gc:cons "out of memory"))
    (heap-set! heap-ptr 'prim)
    (heap-set! (+ 1 heap-ptr) p)
    (heap-set! (+ 2 heap-ptr) 'null)
    (heap-set! (+ 3 heap-ptr) 'unreachable)
    (set! heap-ptr (+ 4 heap-ptr))
    (- heap-ptr 4)))

(define (gc:cons f r)
  (begin
    (when (> (+ heap-ptr 4) (heap-size))
      (mark-and-sweep (get-root-set f r)))
    (heap-set! heap-ptr 'cons)
    (heap-set! (+ 1 heap-ptr) f)
    (heap-set! (+ 2 heap-ptr) r)
    (heap-set! (+ 3 heap-ptr) 'unreachable)
    (set! heap-ptr (+ 4 heap-ptr))
    (- heap-ptr 4)))



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


(define (mark-and-sweep rs)
  (map (lambda (root) (mark (read-root root))) rs))

(define (mark cell)
  (if (gc:flat? cell)
      (heap-set! (+ 3 cell) 'reachable)
      (if (gc:cons? cell)
          (begin
            (heap-set! (+ 3 cell) 'reachable)
            (mark (+ 1 cell))
            (mark (+ 2 cell)))
          (printf "Done~n"))))

(define (is-reachable? c)
  (if (eq? c 'reachable)
      #t
      #f))

(define (free-heap hp)
  (begin
    (if (is-reachable? (heap-ref (+ 3 hp)))
        (heap-set! (+ 3 hp) 'unreachable)
        (begin
          (heap-set! free-ptr hp)
          (set! free-ptr (+ 4 free-ptr)))
        )
    (if (< (+ 4 hp) (heap-size))
        (free-heap (+ 4 hp))
        (printf "Done 2~n"))))




