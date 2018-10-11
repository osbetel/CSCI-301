#lang racket

;Andrew Nguyen, CSCI 301 @ Western Washington University
;Professor Geof Matthews, Lab Assgn. #3
;January 31, 2017

(define length
  (lambda (ls)
    (if (null? ls)
        0
        (+ 1 (length (cdr ls))))))


(define shorter
  ;need a way to iterate indexes
  (lambda (ls1 ls2)
    (if (<= (length ls1) (length ls2))
        ls1
        ls2)))

(define (longer-list a b)
  (and (list? a)
       (list? b)
       (let ll ((aa a) (bb b))
         (display bb)
         (cond
           ((and (null? aa) (null? bb)) #t)
           ((null? aa) b)
           ((null? bb) a)
           (else (ll (cdr aa) (cdr bb)))))))

(define ls1 (make-list 4 'c))
(define ls2 (make-list 7 'c))

(define (shorter-list ls1 ls2)
  
  (if (and (not (null? ls1)) (not (null? ls2)))
      (
       ;(display ls1)
       ;(display ls2)
       ;(newline)
       ;(display (null? ls1))
       ;(display (null? ls2))
       ;(newline)
       ;(newline
       (shorter-list (cdr ls1) (cdr ls2))
      
       )

      ;one of the lists has become null
      (
       (cons ls1 ls2)
       )))