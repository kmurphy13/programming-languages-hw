#lang racket

; Kira Murphy
; Scheme HW 2

; Question 1: Cross product of two sets using fold
; If you replace foldl with foldr or foldr with foldl, it will change the order in which the pairs are outputted.
; Consider (cpf '(1 2 3) '("a" "b" "c"))
; If we switch the inner fold, the numbers will be in increasing order but the letters will be in reverse alphabetical order
; If we switch the outer fold, the numbers will be in decreasing order but the letters will be in alphabetical order
; A precondition is that the list must be nonempty. If one of the lists is empty, the cross product will be null

(define (cpf lst1 lst2)
  (foldr (lambda (x s) (append (foldr (lambda (y r) (cons (list x y) r)) null lst2) s)) null lst1))



; Question 2: Cross product of multiple sets
; Similar to the above example, exchaning foldl for foldr will change the ordering of each list. If we use foldl in both places,
; the first element of each list in the cross product will come from the last list in our list of lists and build backward.
; However, if we use foldr in both places, the first element of each list will come from the first list in our list of lists and build forward.

; Recursive
(define (cartn lst)
  (if (= 2 (length lst))
      (cpf (first lst) (first (rest lst)))
      (map flatten (cpf (first lst) (cartn (rest lst))))))

; I couldn't figure out how to produce the proper answer without flatten and I know we haven't learned about this in class.
; If I remove flatten, the output of the example is: 
;'((1 ("a" "x"))
;  (1 ("a" "y"))
;  (1 ("b" "x"))
;  (1 ("b" "y"))
;  (1 ("c" "x"))
;  (1 ("c" "y"))
;  (2 ("a" "x"))
;  (2 ("a" "y"))
;  (2 ("b" "x"))
;  (2 ("b" "y"))
;  (2 ("c" "x"))
;  (2 ("c" "y"))
;  (3 ("a" "x"))
;  (3 ("a" "y"))
;  (3 ("b" "x"))
;  (3 ("b" "y"))
;  (3 ("c" "x"))
;  (3 ("c" "y")))
; I know that this is incorrect and must have something to do with the way my cpf function is written, but 
; I couldn't figure out how to get it to work any other way. I learned about flatten after reading the Dr. Racket documentation and
; applied my knowledge of map to produce the correct ouput

; Non-recursive
(define (cartnf lst)
  (foldr (lambda (val result)
           (if (null? result)
               val
               (foldr (lambda (first-el r) (append
                                            (if (list? first-el)
                                                (map (lambda (x) (cons x first-el)) val)
                                                (map (lambda (x) (list x first-el)) val)) r))
                      null result)))
           null lst))


; Question 3: Returns the number of occurrences of s in slist.
(define (count-occurrences s slist)
  (if (null? slist)
      0
      (if (eq? s (first slist))
          1
          (if (list? (first slist))
              (+ (count-occurrences s (first slist)) (count-occurrences s (rest slist)))
              (count-occurrences s (rest slist))))))     


; Question 4: Write a function zip that takes two lists and returns a lists of pairwise items.
; A precise and weakest precondition for zip is that the two lists must have the same number of elements
(define (zip lst1 lst2)
  (map (lambda (x y) (list x y)) lst1 lst2))

; Question 5: Dot product
; A preciase and weakest precondition for dp is that the two lists must have the same number of elements and these elements must be numbers
(define (dp lst1 lst2)
  (foldl + 0 (map (lambda (a) (foldl * 1 a)) (zip lst1 lst2))))

; Question 6: The distance between n-points
(define (distance lst1 lst2)
  (sqrt (foldl + 0 (map (lambda (a b) (expt (- a b) 2)) lst1 lst2))))

(define s1 '(1 2 3 4))
(define s2 '(a b c))
(define s3 '(x y))
(cpf s1 s2)
(if (= 24 (length (cartn (list s1 s2 s3)))) "pass" "fail")
(if (= 24 (length (cartnf (list s1 s2 s3)))) "pass" "fail")
(if (= 3 (count-occurrences 'x '((f x) y (((x z) x))))) "pass" "fail")
(if (= 3 (count-occurrences 'x '((f x) y (((x z) () x))))) "pass" "fail")
(if (= 0 (count-occurrences 'w '((f x) y (((x z) x))))) "pass" "fail")
(if (equal? (zip (rest s1) s2)  '((2 a) (3 b) (4 c))) "pass" "fail")
(if (= 60 (dp '(1 2 3 4) '(4 5 6 7))) "pass" "fail")
(if (= 6 (distance '(1 2 3 4) '(4 5 6 7))) "pass" "fail")



