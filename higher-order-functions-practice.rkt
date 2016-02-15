#lang racket

(define a (list 1 2 3 4 5))

(define (inc x)
  (+ x 1))

(map inc a)


(filter odd? a)
(filter even? a)


(filter (lambda (x) (> x 2)) a) ; lambda is the function maker. It filters out all things that are less than two

(foldr + 0 a) ;you need to put in an operation, a base case, and a list

(foldr string-append "" (list "hi " "my " "name " "is " "bob"))

(foldl string-append "" (list "hi " "my " "name " "is " "bob"))
(filter (lambda (x) (> x 2)) a)