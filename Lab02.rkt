#lang racket

;Andrew Nguyen
;W0118 3647


;A symmetric difference is just (A - B) U (B - A)
;The append function in racket/scheme can function as the union
;The removal of duplicates is kind of redundant because it is assumed we are given sets,
;But Racket uses only lists so I used it as insurance against mistaken/possible test duplicates
(define (symmetricDifference a b)
  (remove-duplicates (append (subSet a b) (subSet b a))))

;This function performs an A - B set operation and returns the result
(define (subSet a b)
  (cond [(null? a) '()]     ;if reached end of list (from recursion), return empty list
    [(isEle? (car a) b) (subSet (cdr a) b)]
    (#t (cons (car a) (subSet (cdr a) b)))))
    ;cond executes all statements in its body that have a true condition
    ;So using #t here means the last statement always executes

;Helper function to sub. Used to query whether an element x is in the target list
(define (isEle? x lst)
  (cond [(null? lst) #f]
    [(eq? x (car lst)) #t]
    [#t (isEle? x (cdr lst))]))

;(1^2) + (2^2) + (3^2) + ... + (n^2)
(define (two-LHS n)
  (cond [(= n 0) 0]
    [else (+ (* n n) (two-LHS (- n 1)))]))

;(2n^3 + 3n^2 + n) / 6
(define (two-RHS n)
  (/ (+ (* 2 n n n) (* 3 n n) n) 6))

;(1 + 2 + 3 + ... + n)^2, can use the general term (n^2 + n) / 2 to get the sum of the first n numbers
(define (three-LHS n)
  ((lambda (x) (* x x)) (/ (+ (* n n) n) 2)))

;(1^3) + (2^3) + (3^3) + ... + (n^3)
(define (three-RHS n)
  (cond [(= n 0) 0]
    [else (+ (* n n n) (three-LHS (- n 1)))]))


(define (testall)
  ;test symmetricDifference
  (define a '(1 2 3 4))
  (define b '(3 4 5 6))
  (define c '(once upon a time mary had))
  (define d '(time mary had a little lamb))
  ;final set of a and b should be '(1 2 5 6)
  ;final set of c and d should be '(once upon little lamb)
  (display "symmetricDifference") (newline)
  (display (symmetricDifference a b)) (newline)
  (display (symmetricDifference c d)) (newline) (newline)

  ;test two-LHS and two-RHS
  (display "two-LHS") (newline)
  
  (display (two-LHS 1)) (newline)
  (display (two-LHS 2)) (newline)
  (display (two-LHS 3)) (newline)
  (display (two-LHS 4)) (newline)
  (display (two-LHS 5)) (newline) (newline)

  (display "two-RHS") (newline)
   
  (display (two-RHS 1)) (newline)
  (display (two-RHS 2)) (newline)
  (display (two-RHS 3)) (newline)
  (display (two-RHS 4)) (newline)
  (display (two-RHS 5)) (newline) (newline)

  (display "three-LHS") (newline)
  
  (display (three-LHS 1)) (newline)
  (display (three-LHS 2)) (newline)
  (display (three-LHS 3)) (newline)
  (display (three-LHS 4)) (newline)
  (display (three-LHS 5)) (newline) (newline)

  (display "three-RHS") (newline)
  
  (display (three-RHS 1)) (newline)
  (display (three-RHS 2)) (newline)
  (display (three-RHS 3)) (newline)
  (display (three-RHS 4)) (newline)
  (display (three-RHS 5)) (newline) (newline)

)
  



