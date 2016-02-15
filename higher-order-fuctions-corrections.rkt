;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname higher-order-fuctions-corrections) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Assignment: Higher-Order Functions
; Name : <Eric Seamons>






; check-temps1 : (temps) -> boolean
; temps : (listof number)
; returns true if all measurements are between 5 and 95 degrees celsius, else returns false
(define (check-temps1 temps)
  (foldr
   (lambda (a b)
     (and
       b
       (and (<= a 95) (>= a 5))))
  #t
  temps))

; cons first test case
(check-expect (check-temps1 (list 2)) #f)
; cons second test case
(check-expect (check-temps1 (list 30 70 95)) #t)
; empty test case
(check-expect (check-temps1 empty) #t)

;----------------------------------------------------------------------
; check-temps : (temps low high) -> boolean
; temps : (listof number)
; low : number
; high : number
; returns true if all measurements are between low and high degrees celsius, else returns false
; includes low and high within range

(define (check-temps temps low high)
  (foldr
   (lambda (a b)
     (and
       b
       (and (<= a high) (>= a low))))
  #t
  temps))

; cons first test case
(check-expect (check-temps (list 70) 60 85) #t)
; cons second test case
(check-expect (check-temps (list 30 40 59 86) 60 85) #f)
; cons empty test case
(check-expect (check-temps empty 40 80) #t)

;----------------------------------------------------------------------
; convert : (digits) -> number
; digits : (listof number)
;returns a returns a number that is produced from a list of digits (numbers between 0 and 9) The first digit is the least significant, and so on
(define (convert digits)
  (foldr
   (lambda (a b)
     (+ (* b 10) a))
   0
   digits))

; cons first
(check-expect (convert (list 7)) 7)
; cons second
(check-expect (convert (list 2 8 1)) 182)
; convert empty test case
(check-expect (convert empty) 0)

;----------------------------------------------------------------------
; average-prices : (prices) -> number
; prices (listof number)
; returns average of a list of toy prices. The average is the total of all prices divided by the number of toys

(define (average-price prices)
  (if (empty? prices)
      0
      (/ (foldr + 0 prices) (length prices))))

; cons first test case
(check-expect (average-price (list 7.76)) 7.76)
; cons second test case
(check-expect (average-price (list 3.5 13)) 8.25)
; average price empty test
(check-expect (average-price empty) 0)

;---------------------------------------------------------------------
; convertFC : (fahrenheit) -> (listof number)
; fahrenheit : (listof number)
; returns a list of temperatures converted to Celsius
(define (convertFC fahrenheit)
  (map
   (lambda (x)
     (* (- x 32) (/ 5 9)))
   fahrenheit))
   
; cons first test case
(check-expect (convertFC (list 32)) (list 0))
; cons second test case
(check-expect (convertFC (list 98.6 32)) (list 37 0))
; convertFC empty test
(check-expect (convertFC empty) empty)

;----------------------------------------------------------------------
; eliminate-exp : (and ua ltop) -> (listof number)
; ua : number
; lotp: (listof number)
; returns a list of all toys whose price is less than or equal to ua
(define (eliminate-exp ua lotp)
  (filter
   (lambda (x)
     (<= x ua))
   lotp))

; cons first test cases
(check-expect (eliminate-exp 7.50 (list 8.00)) empty)
(check-expect (eliminate-exp 7.50 (list 6.50)) (list 6.50))
; cons second test cases
(check-expect (eliminate-exp 7.50 (list 7.50 3.75 7.75 8.00 7.51 7.00)) (list 7.50 3.75 7.00))
(check-expect (eliminate-exp 7.50 (list 6.50 8.21)) (list 6.50))
; eliminate-exp empty test
(check-expect (eliminate-exp 7.30 empty) empty)

;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
; compose-func : (and after before) -> (alpha . -> . gamma)
; after : (beta . -> . gamma)
; before : (alpha . -> . beta)
; returns composition before and after

(define (compose-func after before)
  (lambda (x)
    (after (before x))))



; functions used to test compose-func function
(define (inc x) (+ x 1))
(define (square x) (* x x))
(define (dec x) (- x 1))

; compose-func first test case
(check-expect ((compose-func inc square) 3) ((compose inc square) 3))
; compose-func second test case
(check-expect ((compose-func square dec) 8) ((compose square dec) 8))

;---------------------------------------------------------------
; flatten : (lolon) -> (listof number)
; lolon : (listof (listof number))
; produces a list of all the numbers in the element of lolon

(define (flatten lolon)
  (if (empty? lolon)
      empty
      (append
       (first lolon)
       (flatten (rest lolon)))
      ))


(check-expect (flatten (list '())) empty)
(check-expect (flatten (list '() '() '() )) empty)
; empty case
(check-expect (flatten empty) empty)
; cons first
(check-expect (flatten (list (list 6))) (list 6))
; cons second
(check-expect (flatten (list (list 1 2) (list 3 4 5) (list 6))) (list 1 2 3 4 5 6))


;---------------------------------------------------------------
; flatten-foldr : (lolon) -> (listof number)
; lolon : (listof (listof number))
; produces a list of all the numbers in the element of lolon
(define (flatten-foldr lolon)
  (foldr
   (lambda (a b)
    (append a b))
  empty
  lolon))

(check-expect (flatten (list '())) empty)
(check-expect (flatten (list '() '() '() )) empty)
; empty case
(check-expect (flatten empty) empty)
; cons first
(check-expect (flatten (list (list 6))) (list 6))
; cons second
(check-expect (flatten (list (list 1 2) (list 3 4 5) (list 6))) (list 1 2 3 4 5 6))
;---------------------------------------------------------------------

(define (bucket lon)
  (foldr
   (lambda (x y)
     (cond
       [(empty? y) (cons(cons x empty) y)]
       [(not (= x (first (first y))))(cons(cons x empty) y)]
       [(= x (first (first y))) (cons (cons x (first y)) (rest y))]
         )
     )
   empty
   lon))

; empty case
(define case1 empty)
(check-expect (bucket case1) empty)
; cons combines case
(define case2 (list 1))
(check-expect (bucket case2) (list (list 1)))
; cons doesn't combine cons case
(define case3 (list 1 2 3 4))
(check-expect (bucket case3) (list (list 1) (list 2) (list 3) (list 4)))
; cons doesn't combine empty case
(define case4 (list 1 1 2 2 2 3 1 1 1 2 3 3))
(check-expect (bucket case4) (list (list 1 1) (list 2 2 2) (list 3) (list 1 1 1) (list 2) (list 3 3)))

;---------------------------------------------------------------------

; structs
(define-struct person (name birthyear eyecolor father mother))
(define-struct unknown())

; people in Seamons Family Tree without last names
(define Bruce (make-person "Bruce" 1935 'brown (make-unknown) (make-unknown)))
(define Kent (make-person "Kent" 1962 'brown Bruce (make-unknown)))
(define Linda(make-person "Linda" 1960 'brown (make-unknown) (make-unknown)))
(define Eric (make-person "Eric" 1990 'brown Kent Linda))

; people in Seamons Family Tree with last names appended
(define Bruce2 (make-person "Bruce Seamons" 1935 'brown (make-unknown) (make-unknown)))
(define Kent2 (make-person "Kent Seamons" 1962 'brown Bruce2 (make-unknown)))
(define Linda2(make-person "Linda Seamons" 1960 'brown (make-unknown) (make-unknown)))
(define Eric2 (make-person "Eric Seamons" 1990 'brown Kent2 Linda2))

;---------------------------------------------------------------------
; tree-map : (and f tree) -> (or/c unknown person)
; f : (string -> string)
; tree : (or/c unknown person)
; returns a tree where f has been applied to every person's name in tree
(define (tree-map f tree)
  (if (unknown? tree)
       (make-unknown)
       (make-person
        (f (person-name tree))
        (person-birthyear tree)
        (person-eyecolor tree)
        (tree-map f (person-father tree))
        (tree-map f (person-mother tree))
        )))

; unknown case
;(check-expect (tree-map "is excited" (make-unknown)) (make-unknown))
; person second case
(check-expect
 (tree-map (lambda (x) (string-append x " is really happy")) Eric)
 (make-person
 "Eric is really happy"
 1990
 'brown
 (make-person "Kent is really happy" 1962 'brown (make-person "Bruce is really happy" 1935 'brown (make-unknown) (make-unknown)) (make-unknown))
 (make-person "Linda is really happy" 1960 'brown (make-unknown) (make-unknown))))
; person first case
(check-expect
 (tree-map (lambda (x) (string-append x " is sad")) Linda)
 (make-person
 "Linda is sad"
 1960
 'brown
 (make-unknown)
 (make-unknown)))

;---------------------------------------------------------------------
; add-last-name : (and tree lname) -> (or/c unknown person)
; lname : (string)
; tree : (or/c unknown person)
; returns a tree where lname has been appended to every person's name in tree

(define (add-last-name tree lname)
  (tree-map
   (lambda (x)
     (string-append x " " lname))
   tree))

;empty case
(check-expect (tree-map "Seamons" (make-unknown)) (make-unknown))
; person first case
(check-expect (add-last-name Eric "Seamons") Eric2)
; person second case
(check-expect (add-last-name Linda "Seamons") Linda2)
(check-expect (add-last-name (make-unknown) "Seamons") (make-unknown))