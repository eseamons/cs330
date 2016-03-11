#lang lazy


(define division_by_zero (/ 1 0))

(define division_by_zero_2 (/ division_by_zero 0))
(define division_by_zero_3 (/ division_by_zero_2 0))


;(printf division_by_zero_3)