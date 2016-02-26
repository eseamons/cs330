#lang racket

(define (filter/k f l k)
  (if (empty? l)
      (display (k empty))
      (if (f (first l))
          (filter/k f (rest l) (lambda (x) (append (k x) (list (first l)) )))
          (filter/k f (rest l) (lambda (x) (k x)))
          )))

(define (less-than-three x)
   (< x 3))


(filter/k (lambda (x) (< x 5)) '(1 2 3 4 5 6 7 8 9 10 11) (lambda (x) x))

(filter/k less-than-three (cons 1 (cons 4 empty)) (lambda (x) x))