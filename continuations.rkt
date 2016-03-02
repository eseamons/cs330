#lang racket


 ; filter: (x -> bool) (listof x) -> (listof x)
(define (filter f l)
  (cond
    [(empty? l) empty]
    [else (cond
            [(f (first l)) (cons (first l)
                                 (filter f (rest l)))]
            [else (filter f (rest l))])]))


; filter/k: (x receiver -> doesn't) (listof x) receiver -> doesn't
(define (filter/k f/k l k)
  (if (empty? l)
      (k empty)
      (f/k (first l) (lambda (first-result)
                       (filter/k f/k (rest l)
                                 (lambda (rest-results)
                                   (if first-result
                                       (k (cons (first l) rest-results))
                                       (k rest-results))))))))

(define (less-than-three/k x k)
  (k (< x 3)))

(filter/k less-than-three/k (list 1 2 3 4 5 6) display)