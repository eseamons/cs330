#lang lazy


;testing function for lazy racket
(define print-only-errors #f)
(define (test l r)
  (if (equal? l r)
      (if print-only-errors
          ()
          (printf "Test Passed~n"))
      (printf "Test Failed.~nActual:   ~S ~nExpected: ~S~n" l r)))

;--------------------------------------------------------------------------------------------------


; Returns the prefix of l such that for all elements p returns true. This is not filter.
(define (take-while p l)
  (while-loop p empty l))

; while loop that will add item to the list while item meets condition
(define (while-loop condition? new-list old-list)
  (if (or
       (empty? old-list)
       (not (condition? (first old-list))))
      new-list
    (while-loop condition? (append new-list (list (first old-list))) (rest old-list))
    ))

;--------------------------------------------------------------------------------------------------
  
; Returns true if n is prime
(define (prime? num)
  (cond
    [ (zero? num) #f]
    [ (= 1 num) #f]
    [else
     (andmap
      (lambda (x)
        (if (and
             (not (= x num))
             (integer? (/ num x)))
            #f
            #t))
      (create-list-from-2-to-n num empty))]))

;----------------------------------------------------------------------------------------------------


(define (create-list-from-2-to-n n lst)
  (if (= (length lst) (- n 1))
      lst
      (create-list-from-2-to-n n (cons (+ 2 (length lst)) lst))))


(define (all-integers-from n)
  (cons n (all-integers-from (+ n 1))))

(define (multiples-of-two-from n)
  (cons (* 2 n) (multiples-of-two-from (+ n 1))))

(define multiples-of-two-from-1 (multiples-of-two-from 1))

(define nums (all-integers-from 1))

(define evens (filter even? nums))

(define my-list (create-list-from-2-to-n 10 empty))

(define primes (take-while prime? nums))




;---------------------------------------------------------------------------------------------------
; take while tests

; an empty case test
(test (take-while even? empty) empty)
; non-empty case test
(test (take-while odd? (list 1 3 4)) (list 1 3))
; infinite input case test
(test (take-while (lambda (n) (< n 20)) nums)
      (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19))
;(test (take 100 (take-while even? nums)) (take 100 multiples-of-two-from-1))