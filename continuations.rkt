#lang racket
; filter: (x -> bool) (listof x) -> (listof x)
(define (my-filter f l)
  (cond
    [(empty? l) empty]
    [else (cond
            [(f (first l)) (cons (first l)
                                 (my-filter f (rest l)))]
            [else (my-filter f (rest l))])]))
 ;(my-filter (lambda(x) (< x 5)) (list 1 2 3 4 5 6 7 8 9 10)) 


 ;filter/k: (x receiver -> doesn't) (listof x) receiver -> doesn't
(define (filter/k f l k)
  (if (empty? l)
      (display (k empty))
      (if (f (first l))
          (filter/k f (rest l) (lambda (x) (cons (first l) (k x))))
          (filter/k f (rest l) (lambda (x) (k x)))
          )))

(define (make-list item)
  (list item))

(define (filter/k2 f l k)
  (if (empty? l)
      (display (k empty))
      (if (f (first l))
          (filter/k2 f (rest l) (lambda (x) (append (k x) (make-list (first l)) )))
          (filter/k2 f (rest l) (lambda (x) (k x)))
          )))


;(filter/k (lambda (x) (< x 5)) '(1 2 3 4 5 6 7 8 9 10 11) (lambda (x) x))
;(filter/k2 (lambda (x) (< x 5)) '(1 2 3 4 5 6 7 8 9 10 11) (lambda (x) x))


(define (less-than-three x)
   (< x 3))
(filter/k2 less-than-three
        (cons 1 (cons 4 empty)) (lambda (x) x))




#|
(define (tally/k item-list k)
  (if (empty? item-list)
      (k 0)
      (web-read/k (generate-item-cost-prompt (first item-list))
                  (lambda (first-cost)
                    (tally/k (rest item-list)
                             (lambda (rest-cost)
                               (k (+ first-cost rest-cost))))))))
|#

#|

(define lst
  '( 1 2 3 4 5))

((lambda (x) x) '(1 2 3 4 5))


((lambda (x)
   (cons 1 ((lambda (x)
       (cons 2 ((lambda (x)
              (cons 3 ((lambda (x)
                   (cons 4 x)) x))) x))) x))) empty)



((lambda (x)
   (cons 1 ((lambda (x)
     (cons 2 ((lambda (x)
         (cons 3 x)) x))) x))) empty)

((lambda (x)
   (cons 1 ((lambda (x)
         (cons 2 x)) x))) empty)
|#