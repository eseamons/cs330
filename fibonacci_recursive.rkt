;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fibonacci_recursive) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (fib x)
  (cond
    ((= x 0) 0)
    ((= x 1) 1)
    (else
     (+
      (fib (- x 1))
      (fib (- x 2)))
     )
  
  ))