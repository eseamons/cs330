;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lists-and-trees-final_11_20) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Assignment: Lists and Trees
; Name: Eric Seamons

; check-temps1 : (temps) -> boolean
; temps : (listof number)
; returns true if all measurements are between 5 and 95 degrees celsius, else returns false
; includes 5 and 95 within range

(define (check-temps1 temps)
  (if (empty? temps)
      #t
      (and
       (and (<= (first temps) 95)
                (>= (first temps) 5)) 
           (check-temps1 (rest temps)))))

; all temperatures within 5 and 95
(check-expect (check-temps1 (list  5 35 94 6 95)) #t)
; all temperatures outside 5 and 95 range
(check-expect (check-temps1 (list 1 2 100 120)) #f)
; one temperature outside 5 and 95 range
(check-expect (check-temps1 (list 30 70 100)) #f)


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
  (if (empty? temps)
      #t
      (and
       (and (<= (first temps) high)
                (>= (first temps) low)) 
           (check-temps (rest temps) low high))))

; all temps within range of 25 and 45
(check-expect (check-temps (list 25 30 40 45) 25 45) #t)
; all temps outside range of 60 and 85
(check-expect (check-temps (list 30 40 59 86) 60 85) #f)

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
  (if (empty? digits)
      0
      (+ (first digits)
         (* 10 (convert (rest digits)))
         )))

; test 1
(check-expect (convert (list 3 1 7)) 713)
; test 2
(check-expect (convert (list 7 4 1 8)) 8147)
; test 3
(check-expect (convert (list 7 8 9 1 3)) 31987)

; cons first
(check-expect (convert (list 7)) 7)
; cons second
(check-expect (convert (list 2 8 1)) 182)
; convert empty test case
(check-expect (convert empty) 0)

;---------------------------------------------------------------------
; sum-prices : (prices) -> number
; prices : (listof number)
; returns the sum of all the prices of toys in the list

(define (sum-prices prices)
  (if (empty? prices)
      0
      (+ (first prices) (sum-prices (rest prices)))))

; sum-price test 1
(check-expect (sum-prices (list 1 2 3 4)) 10)
; sum-price test 2
(check-expect (sum-prices (list 10 20 5 4)) 39)

; cons first
(check-expect (sum-prices (list 20)) 20)
; cons second
(check-expect (sum-prices (list 20 13.5)) 33.5)
; sum-prices empty test
(check-expect (sum-prices empty) 0)
;----------------------------------------------------------------------
; average-prices : (prices) -> number
; prices (listof number)
; returns average of a list of toy prices. The average is the total of all prices divided by the number of toys

(define (average-price prices)
  (if (empty? prices)
      0
      (/ (sum-prices prices) (length prices))))


; average price test 1
(check-expect (average-price (list 9.5 11.5)) 10.5)
; average price test 2
(check-expect (average-price (list 10 3 4 6)) 5.75)

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
  (if (empty? fahrenheit)
      empty
      (cons
       (* (- (first fahrenheit) 32) (/ 5 9))
       (convertFC (rest fahrenheit)))))

; convertFC test 1
(check-expect (convertFC (list 98.6 32)) (list 37 0))
; convertFC test 2
(check-expect (convertFC (list 329 428 185)) (list 165 220 85))

; cons first test case
(check-expect (convertFC (list 32)) (list 0))
; cons second test case
(check-expect (convertFC (list 98.6 32)) (list 37 0))
; convertFC empty test
(check-expect (convertFC empty) empty)

;---------------------------------------------------------------------
; eliminate-exp : (and ua ltop) -> (listof number)
; ua : number
; lotp: (listof number)
; returns a list of all toys whose price is less than or equal to ua
(define (eliminate-exp ua lotp)
  (if (empty? lotp)
      empty
      (if (<= (first lotp) ua)
          (cons (first lotp) 
                (eliminate-exp ua (rest lotp)))
          (eliminate-exp ua (rest lotp)))))

; eliminate-exp test 1
(check-expect (eliminate-exp 7.50 (list 7.50 3.75 7.75 8.00 7.51 7.00)) (list 7.50 3.75 7.00))
; eliminate-exp test 2
(check-expect (eliminate-exp 7.50 (list 6.50 8.21)) (list 6.50))

; cons first test cases
(check-expect (eliminate-exp 7.50 (list 8.00)) empty)
(check-expect (eliminate-exp 7.50 (list 6.50)) (list 6.50))
; cons second test cases
(check-expect (eliminate-exp 7.50 (list 7.50 3.75 7.75 8.00 7.51 7.00)) (list 7.50 3.75 7.00))
(check-expect (eliminate-exp 7.50 (list 6.50 8.21)) (list 6.50))
; eliminate-exp empty test
(check-expect (eliminate-exp 7.30 empty) empty)
;---------------------------------------------------------------------
; suffixes : (l) -> (listof list)
; l : list
; returns a list of suffixes of l
(define (suffixes l)
  (if (empty? l)
      (cons empty empty)
      (cons l (suffixes (rest l)))))

; cons first test case
(check-expect (suffixes (list 'a)) (list (list 'a) '()))
; cons second test case
(check-expect (suffixes (list 'a 'b 'c 'd))
              (list (list 'a 'b 'c 'd) (list 'b 'c 'd) (list 'c 'd) (list 'd) '()))
(check-expect (suffixes (list 'f 'g 'h 'i))
              (list (list 'f 'g 'h 'i) (list 'g 'h 'i) (list 'h 'i) (list 'i) '()))
; suffixes empty test
(check-expect (suffixes empty) (cons empty empty))
;---------------------------------------------------------------------

; structs
(define-struct person (name birthyear eyecolor father mother))
(define-struct unknown())

; people in Seamons Family Tree
(define Bruce (make-person "Bruce Seamons" 1935 'blue (make-unknown) (make-unknown)))
(define Kent (make-person "Kent Seamons" 1962 'green Bruce (make-unknown)))
(define Linda(make-person "Linda Seamons" 1960 'brown (make-unknown) (make-unknown)))
(define Eric (make-person "Eric Seamons" 1990 'brown Kent Linda))

;---------------------------------------------------------------------
; count-persons : (ftree) -> number
; ftree : (or/c unknown person)
; returns the number of people in a family tree
(define (count-persons ftree)
  (if (unknown? ftree)
      0
      (+
       1
       (count-persons (person-father ftree))
       (count-persons (person-mother ftree)))))

; count-persons test 1
(check-expect (count-persons Eric) 4)

; cons first test case
(check-expect (count-persons Linda) 1)
; cons second test case
(check-expect (count-persons Kent) 2)
; unknown test case
(check-expect (count-persons (make-unknown)) 0)

;---------------------------------------------------------------------(define (count-persons ftree)
; sum-ages : (ftree) -> number
; ftree : (or/c unknown person)
; returns sum of the ages of the people in the family tree
; this is an extra function used in average-age
(define (sum-ages ftree)
  (if (unknown? ftree)
      0
      (+
       (- 2016 (person-birthyear ftree))
       (sum-ages (person-father ftree))
       (sum-ages (person-mother ftree)))))


(check-expect (sum-ages Bruce) 81)
(check-expect (sum-ages Kent) 135)

; cons first test case
(check-expect (sum-ages Linda) 56)
; cons second test case
(check-expect (sum-ages Eric) 217)
; unknown test case
(check-expect (sum-ages (make-unknown)) 0)

;---------------------------------------------------------------------
; average-age : (ftree) -> number
; ftree : (or/c unknown person)
; returns the average age of all the people in the family tree (Assume the current year is 2016)
(define (average-age ftree)
  (if (unknown? ftree)
      0
      (/ (sum-ages ftree) (count-persons ftree))))

; average-age test 1
(check-expect (average-age Kent) 67.5)


; cons first test case
(check-expect (average-age Linda) 56)
; cons second test case
(check-expect (average-age Eric) 54.25)
; average-age unknown test
(check-expect (average-age (make-unknown)) 0)

;----------------------------------------------------------------------
; eye-colors (ftree) -> (listof symbol)
; ftree : (or/c unknown person)
; returns a list of all eye colors in family tree
; an eye color may occur more than once in the list
(define (eye-colors ftree)
  (if (unknown? ftree)
      empty
      (cons
       (person-eyecolor ftree)
       (append
        (eye-colors (person-father ftree))
        (eye-colors (person-mother ftree))))))

(check-expect (eye-colors Eric) (list 'brown 'green 'blue 'brown))

; cons first test case
(check-expect (eye-colors Linda) (list 'brown))
; cons second test case
(check-expect (eye-colors Kent) (list 'green 'blue))
; empty test case
(check-expect (eye-colors (make-unknown)) empty)