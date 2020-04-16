#lang racket
; Author: Kira Murphy
; Programming Languages Homework

; Problem 1 - Square Root
(define (mysqrt n)
  (let ((epsilon (* 1(expt 10 -9))))
  (define (mysqrt-1 n r)
     (if (< (abs (- (* r r) n)) epsilon)
         (exact->inexact r)
         (mysqrt-1 n (/ (+ r (/ n r)) 2))
         )
    )
   (mysqrt-1 n n)
))

; Problem 2 - Tribonacci Sequence
(define (trib n)
  (define (trib-1 n1 n2 n3 a)
  (if (= n a)
      (+ (+ n1 n2) n3)
      (trib-1 n2 n3 (+ (+ n1 n2) n3) (+ a 1)
      )
    )
  )
  (trib-1 0 1 1 3)
)


; Problem 3 - Dictionary Implementation

; Test dictionary
(define sample-dict '(("42" 'lambda) ("a" 43) ("banana" (1 2 3)) ("dog" "woof")))

; Insert key/value pair into dictionary
(define (insert key value dict)
  (if (null? dict)
      (cons (list key value) dict)
      (if (string<? key (first (first dict)))
          (cons (list key value) dict)
          (if (string=? (first (first dict)) key)
              (cons (list key value) (rest dict))
              (cons (first dict) (insert key value (rest dict)))))))

; Lookup value for a given key
(define (lookup key dict def-val)
  (if (null? dict)
      def-val
      (if (string=? (first (first dict)) key)
          (first(rest (first dict)))
          (lookup key (rest dict) def-val))))

  
; Creates a dictionary of character frequencies for a string
(define (insert-string str dict)
  (let ((str-list (string->list str)))
    (define (insert-string-1 lst dict)
      (if (null? lst)
          dict
          (if (null? (lookup (string (first lst)) dict null))
              (insert-string-1 (rest lst) (insert (string(first lst)) 1 dict))
              (insert-string-1 (rest lst) (insert (string(first lst)) (+ 1 (lookup (string (first lst)) dict null)) dict))
      )
    ))
    (insert-string-1 str-list dict)
))

; Creates a dictionary of character frequencies for a file
(define (insert-lines lst-lines dict)
  (if (null? lst-lines)
      dict
      (insert-lines (rest lst-lines) (insert-string (first lst-lines) dict))))


      
          
      