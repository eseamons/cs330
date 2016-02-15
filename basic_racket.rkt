;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname basic_racket) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Assignment: Basic Racket
;Name: <Eric Seamons>


; sum-coins : (and pennies nickels dimes quarters) -> number
; returns sum of pennies, nickels, dimes, and quarters in a bag
(define (sum-coins pennies nickels dimes quarters)
  
  (/
  (+ (* pennies 1)
     (* nickels 5)
     (* dimes 10)
     (* quarters 25)) 100)
 )

; pennies test
(check-expect (sum-coins 22 0 0 0) .22)
; nickles test
(check-expect (sum-coins 0 3 0 0) .15)
; dimes test
(check-expect (sum-coins 0 0 7 0) .70)
; quarters test
(check-expect (sum-coins 0 0 0 3) .75)
; combined case tests
(check-expect (sum-coins 7 2 3 4) 1.47)
(check-expect (sum-coins 5 2 1 1) 0.5)
(check-expect (sum-coins 4 5 3 1) 0.84)

;------------------------------------------------------------------------
; area-cylinder : (and base-radius height) -> number
; returns the surface area of a cylinder
(define (area-cylinder base-radius height)
  (+
   (* 2 pi base-radius height)
   (* 2 pi (expt base-radius 2)))
 )
; tests
(check-within (area-cylinder 1 3) 25.13 .01)
(check-within (area-cylinder 5 4) 282.74 .01)

;------------------------------------------------------------------------
; tax : (gross-pay) -> number
; returns the amount of tax owed according to the gross pay
(define (tax gross-pay)
  (cond
    ((<= gross-pay 240) 0)
    ((and (<= gross-pay 480) (> gross-pay 240)) (* gross-pay .15))
    ((> gross-pay 480) (* gross-pay .28))
   ))

; tests where gross-pay is between 0 and 240
(check-expect (tax 150) 0)
(check-expect (tax (gross-pay 25)) 45) ; in this case gross pay is 300
; test where gross-pay is exactly 240
(check-expect (tax 240) 0)
; test where gross-pay is between 240 and 480
(check-expect (tax 241) 36.15)
; test where gross-pay is between 240 and 480
(check-expect (tax 250) 37.5)
; test where gross-pay is exactly 480
(check-expect (tax 480) 72)
; test where gross-pay is greater than 480
(check-expect (tax 496) 138.88)
; test where gross-pay is greater than 480
(check-expect (tax 567) 158.76)


;-----------------------------------------------------------------------
; gross-pay (hours-worked) -> number
; returns the gross pay of an employee, which is 12 multiplied by hours worked
(define (gross-pay hours-worked)
  (* hours-worked 12))

; test gross-pay
(check-expect (gross-pay 25) 300)
(check-expect (gross-pay 12) 144)
;------------------------------------------------------------------------
; netpay : (hours-worked) -> number
; returns the net pay of an employee, which is the gross pay minus the tax Assume the hourly pay rate is $12
(define (netpay hours-worked)
  (- (gross-pay hours-worked) (tax (gross-pay hours-worked)))
 )

; test netpay
(check-expect (netpay 25) 255)
(check-expect (netpay 45) 388.8)
;--------------------------------------------------------------------------
; what-kind : (and a b c) -> symbol
; returns whether or not the equation is degenerate and if not, how many solutions the equation has
(define (what-kind a b c)
  (cond
    ((= a 0)
     'degenerate)
    ((< (- (expt b 2) (* 4 a c)) 0)
     'none)
    ((= (- (expt b 2) (* 4 a c)) 0)
     'one)
    ((> (- (expt b 2) (* 4 a c)) 0)
     'two)
    ))

; test degenerate
(check-expect (what-kind 0 2 3) 'degenerate)
(check-expect (what-kind 0 2 0) 'degenerate)
; test none
(check-expect (what-kind 1 0 3) 'none)
(check-expect (what-kind 8 4 3) 'none)
; test one solution
(check-expect (what-kind 4 8 4) 'one)
(check-expect (what-kind 1 2 1) 'one)
; text two solutions
(check-expect (what-kind 3 8 5) 'two)
;---------------------------------------------------------------------------
; hours-to-secs : (time-hr) -> number
; returns conversion of hours to seconds
(define (hours-to-secs time-hr)
  (* time-hr 60 60))
;---------------------------------------------------------------------------
;mins-to-secs : (time-min) -> number
;returns conversion of minutes to seconds
(define (mins-to-secs time-min)
  (* time-min 60))

;mins-to-sec test case
(check-expect (mins-to-secs 3) 180)
;hours-to-secs test case
(check-expect (hours-to-secs 1) 3600)
;---------------------------------------------------------------------------
; time will be in military time
(define-struct time (hours minutes seconds))
(define first-time (make-time 11 15 0))
(define second-time (make-time 5 45 0))
(define third-time (make-time 14 30 0))
(define fourth-time (make-time 2 21 29))
(define fifth-time (make-time 1 40 20))
; time-diff : (and time1 time2) -> number
;returns number of seconds from time1 and time2
;time1 should be larger than time2
(define (time-diff time1 time2)
  (-
   (+ (hours-to-secs (time-hours time1)) (mins-to-secs (time-minutes time1)) (time-seconds time1))
   (+ (hours-to-secs (time-hours time2)) (mins-to-secs (time-minutes time2)) (time-seconds time2))
   ))

; time-diff test case
(check-expect (time-diff fourth-time fifth-time) 2469)
(check-expect (time-diff first-time second-time) 19800)
;---------------------------------------------------------------------------
; these are the definitions for the shape and position structs
; the position starts at the top left
; the y increments as you go farther down
; the x increments as you go farther to the right
(define-struct position (x y))
(define-struct circ (center radius)) ; center : position?
(define-struct square (upper-left length)) ; upper-left: position?
(define-struct rect (upper-left width height)) ; upper-left: position?
;--------------------------------------------------------------------------
; area : (or circle square rect) -> number
; returns area of a shape depending on if it is a circle, square, or rectangle
(define (area shape)
  (cond
    ((circ? shape)
     (* pi (expt (circ-radius shape) 2)))
    ((square? shape)
     (expt (square-length shape) 2))
    ((rect? shape)
     (* (rect-width shape) (rect-height shape) )))
  )

;shape definitions
(define circle (make-circ (make-position 50 50) 2))
(define circle2 (make-circ (make-position 25 30) 5))
(define square1 (make-square (make-position 0 0) 6))
(define rect1 (make-rect (make-position 50 50) 5 7))
(define point (make-position 20 30))

; circle area tests
(check-within (area circle) 12.57 .01)
(check-within (area circle2) 78.54 .01)

; square area tests
(check-expect (area square1) 36)
(check-expect (area (make-square 23 11)) 121)

; rectangle area tests
(check-expect (area rect1) 35)

;--------------------------------------------------------------------------
; translate-shape : ((or circ square rect) delta) -> (or/c circle square rect)
; returns a shape whose key position is moved by delta pixels in the x direction
(define (translate-shape shape delta)
  (cond
    ((circ? shape)
     (make-circ
      (make-position
      (+ (position-x (circ-center shape)) delta)
      (position-y (circ-center shape)))
      (circ-radius shape)
      )
     )
    ((rect? shape)
     (make-rect
      (make-position
      (+ (position-x (rect-upper-left shape)) delta)
      (position-y (rect-upper-left shape)))
      (rect-width shape)
      (rect-height shape))
     )
    ((square? shape)
     (make-square
      (make-position
      (+ (position-x (square-upper-left shape)) delta)
      (position-y (square-upper-left shape)))
     (square-length shape)))
   ))
    

; square transition case test
(check-expect
(position-x (square-upper-left (translate-shape square1 15)))
 15)
; rectangle transition case test
(check-expect
(position-x (rect-upper-left (translate-shape rect1 10)))
 60)
; circle transition case test
(check-expect
(position-x (circ-center (translate-shape circle2 25)))
 50)
;---------------------------------------------------------------------------
; in-square? : (square position) -> boolean
; returns true if point in square, else returns false
(define (in-square? sqr p)
  (if
   (and
    (and
     (>= (position-x p)
         (position-x (square-upper-left sqr)))
     (<= (position-x p)
         (+ (position-x (square-upper-left sqr))
            (square-length sqr)))
     )
    (and
     (>= (position-y p)
         (position-y (square-upper-left sqr)))
     (<= (position-y p)
         (+ (position-y (square-upper-left sqr))
            (square-length sqr)))
     ))
   #t
   #f
   )
   
   )
;--------------------------------------------------------------------------
; in-square? : (rect position) -> boolean
; returns true if point in rectangle, else returns false
(define (in-rect? r p)
  (if
   (and
    (and
     (>= (position-x p)
         (position-x (rect-upper-left r)))
     (<= (position-x p)
         (+ (position-x (rect-upper-left r))
            (rect-width r)))
     )
    (and
     (>= (position-y p)
         (position-y (rect-upper-left r)))
     (<= (position-y p)
         (+ (position-y (rect-upper-left r))
            (rect-height r)))
     ))
   #t
   #f
   )
   
   )

; point inside rect
(define in-rect-point (make-position 55 57))
; points outside rect
(define out-rect-point (make-position 56 57))
(define out-rect-point2 (make-position 56 58))
;-------------------------------------------------------------------------
; in-circ? : (circle position) -> boolean
; returns true if point in circle, else returns false
(define (in-circ? c p)
  (if
   ( <=
     (+
      (expt (- (position-x p) (position-x (circ-center c))) 2)
      (expt (- (position-y p) (position-y (circ-center c))) 2)
      )
     (expt (circ-radius circle) 2)
     )
   #t
   #f
   )
  
  )


  
;--------------------------------------------------------------------------
; in-shape? : ((or circ square rect) position) -> boolean
; returns true if point in shape, else returns false
(define (in-shape? shape p)
  (cond
    ((circ? shape) (in-circ? shape p))
    ((square? shape) (in-square? shape p))
    ((rect? shape) (in-rect? shape p))
    ))

 ; points inside square
(define in-square-point (make-position 3 6))
(define in-square-point2 (make-position 6 6))
(define in-square-point3 (make-position 0 0))
 ; point outside square
(define out-square-point (make-position 6 8))

; in square test cases
(check-expect (in-shape? square1 in-square-point) #t)
(check-expect (in-shape? square1 in-square-point2) #t)
(check-expect (in-shape? square1 in-square-point3) #t)
; outside square test cases
(check-expect (in-shape? square1 out-square-point) #f)

; in rect test case
(check-expect (in-shape? rect1 in-rect-point) #t)
; outside rect test cases
(check-expect (in-shape? rect1 out-rect-point) #f)
(check-expect (in-shape? rect1 out-rect-point2) #f)

; point inside circle
(define in-circ-point (make-position 50 52))

; inside circle test case
(check-expect (in-shape? (make-circ (make-position 50 50) 2) (make-position 50 52)) #t)

; outside circle test case
(check-expect (in-shape? (make-circ (make-position 50 50) 2) (make-position 53 52)) #f)