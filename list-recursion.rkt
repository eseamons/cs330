;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname list-recursion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (remove-odd lst)
  (if (not (empty? (rest lst)))
      (if (even? (first lst))
          (cons (first lst)
                (remove-odd (rest lst)))
          (remove-odd (rest lst)))
      (if (even? (first lst))
          lst
          empty)))

(define (keep-even lst)
  (if (empty? lst)
      empty
      (if (even? (first lst))
          (cons (first lst)
                (remove-odd (rest lst)))
          (remove-odd (rest lst)))))

(define (keep-cond condition lst)
  (if (not (empty? (rest lst)))
      (if (condition (first lst))
          (cons (first lst) (keep-cond condition (rest lst)))
          (keep-cond condition (rest lst)))
      (if (condition (first lst))
          lst
          empty)))

(define (keep-cond2 condition lst)
  (if (empty? lst)
      empty
      (if (condition (first lst))
          (cons (first lst) (keep-cond2 condition (rest lst)))
          (keep-cond2 condition (rest lst)))))

(define mylist (list 2 4 6 8 10 11 12 13 14 15 17 19 21 23 24 26))