;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname stuct) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct person (name age))

(define you
  (make-person "Brilliant student" 23))

(define me
  (make-person "Pretty good professor" 39))

;(person-name me)


(define-struct student (name grade major))
(define-struct faculty (name department))

; is-CS : (or student faculty) -> boolean
;returns whether or not a student is a CS major or fac
(define (is-CS? person)
  (cond
    ((student? person) (string=? (student-major person) "CS"))
    ((faculty? person) (string=? (faculty-department person) "CS"))
    (else #f)))

(define frank (make-student "frank" "a" "CS"))
(define bob (make-student "Bob" "b-" "physics"))
(define david (make-faculty "david" "CS"))

(is-CS? bob)