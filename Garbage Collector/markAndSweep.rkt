#lang plai/collector

(define free-ptr 'uninitialized-free-ptr) ;points to first item in free list
(define prev-free-ptr 'null)
(define next-free-ptr 'uninitialized-next-free-ptr)
(define free-ptr-set #f)
(define temp 0)

(define (init-allocator)
  (begin
    (set! free-ptr 0)
    (initialize-free-list 0)))

;(initialize-free-list hp) → void?
;  hp : location?
;Sets the entire heap to be the free list to begin allocating
(define (initialize-free-list hp)
  (begin
    (heap-set! hp 'free)
    (heap-set! (+ 3 hp) 'empty)
    (when (not (eq? prev-free-ptr 'null))
      (heap-set! (+ 3 prev-free-ptr) hp))
    (set! prev-free-ptr hp)
    (if (< (+ 8 hp) (heap-size))
      (initialize-free-list (+ 4 hp))
      (begin
        (set! prev-free-ptr 'null)))))

(define (gc:alloc-flat p)
  (begin
    (when (eq? free-ptr 'empty)
      (begin
        (when (procedure? p)
          (mark-and-sweep (procedure-roots p)))
        (mark-and-sweep (get-root-set))
        (free-heap 0)))
    (heap-set! free-ptr 'prim)
    (heap-set! (+ 1 free-ptr) p)
    (heap-set! (+ 2 free-ptr) 'null)
    (set! next-free-ptr (heap-ref (+ 3 free-ptr)))
    (heap-set! (+ 3 free-ptr) 'unreachable)
    (set! temp free-ptr)
    (set! free-ptr next-free-ptr)
    temp))

(define (gc:cons f r)
  (begin
    (when (eq? free-ptr 'empty)
      (mark-and-sweep (get-root-set f r))
      (free-heap 0))
    (heap-set! free-ptr 'cons)
    (heap-set! (+ 1 free-ptr) f)
    (heap-set! (+ 2 free-ptr) r)
    (set! next-free-ptr (heap-ref (+ 3 free-ptr)))                
    (heap-set! (+ 3 free-ptr) 'unreachable)
    (set! temp free-ptr)
    (set! free-ptr next-free-ptr)
    temp))

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

;(mark-and-sweep rs) → void?
;  rs : (listof root?)
;Goes through the entire heap and marks anything recursively
;that is reacheable from the root set
(define (mark-and-sweep rs)
  (map (lambda (root) (mark (read-root root))) rs))

;(mark cell) → void?
;  cell : location
;Marks a cell as reachable, if its a procedure it
;recursively marks its roots, if its a cons, it marks
;first and rest
(define (mark cell)
  (begin
    (heap-set! (+ 3 cell) 'reachable)
    (cond
      [(procedure? cell)
       (mark-and-sweep (procedure-roots cell))]
      [(gc:cons? cell)
       (begin
         (mark (heap-ref (+ 1 cell)))
         (mark (heap-ref (+ 2 cell))))])))

;(is-reachable? c) → void?
;  c : symbol?
;Returns true if c is reachable
(define (is-reachable? c)
  (if (eq? c 'reachable)
      #t
      #f))

;(free-heap hp) → void?
;  hp : location?
;Iterates the heap and frees all the memory that is not marked
;as reachable and puts it into the free list. If an object is reachable
;it is set as unreachable
(define (free-heap hp)
  (begin
    (if (is-reachable? (heap-ref (+ 3 hp)))
        (heap-set! (+ 3 hp) 'unreachable)
        (begin
          (when (eq? free-ptr 'empty)
            (set! free-ptr hp))
          (heap-set! hp 'free)
          (heap-set! (+ 3 hp) 'empty)
          (when (not (eq? prev-free-ptr 'null))
            (heap-set! (+ 3 prev-free-ptr) hp))
          (set! prev-free-ptr hp)))
    (if (< (+ 8 hp) (heap-size))
        (free-heap (+ 4 hp))
        (begin
          (set! prev-free-ptr 'null)))))