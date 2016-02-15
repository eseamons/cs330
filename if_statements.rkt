;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname if_statements) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (adult? x)
  (if
    (>= x 18)
    #true
    #false)
  )

(define (self-sufficient? x)
  (if
    (>= x 10000)
    #true
    #false)
  )

(define (my-match? a b)
  (if
    (and
     (self-sufficient? a)
     (adult? b))
    #true
    #false)
  )

(check-expect  (my-match? 10000000 18) #true)
(check-expect  (my-match? 10000 18) #true)
(check-expect  (my-match? 1000 18) #false)
(check-expect  (my-match? 9999 18) #false)
(check-expect  (my-match? 10000 17) #false)
(check-expect  (my-match? 9999 19) #false)
(check-expect  (my-match? 10001 19) #true)

(define a -200)
(cond
  [(< a 0) -1]
  [(> a 0) 1]
  [else 0]
)