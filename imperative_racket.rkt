#lang racket

(require data/gvector)

(define my-vector #("Eric" "Ale" "Ryan" "Katrina"))

(for ([i (in-range 0  (vector-length my-vector))])
    (printf "Value at ref ~s: ~s~n" i (vector-ref my-vector i)))
(printf "~n")

(define fav-restaurants #("Cafe Rio" "Thai Village" "Pizza Pie Cafe" "Texas Roadhouse"))

(define (get-fave-rest index)
  (vector-ref fav-restaurants index))
(define (get-rest-num)
  (vector-length fav-restaurants))

(for ([i (in-range 0 (get-rest-num))])
  (printf "~s~n" (get-fave-rest i)))

