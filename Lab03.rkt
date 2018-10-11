#lang racket

;Andrew Nguyen
;W0118 3647

;;;;;;;;; LAB 2 ;;;;;;;;;;

;A symmetric difference is just (A - B) U (B - A)
;The append function in racket/scheme can function as the union
;The removal of duplicates is kind of redundant because it is assumed we are given sets,
;But Racket uses only lists so I used it as insurance against mistaken/possible test duplicates
(define (symmetric-difference a b)
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

;;;;;;;;;; END LAB 2 ;;;;;;;;;;

;;;;;;;;;; LAB 3 ;;;;;;;;;;;;;;


;expects input and output files in the format "input.txt" and "output.txt"
;Input file has numbers in a sequence like "1 2 3 4 5"
(define (sum-up input output)
  ;two LHS and RHS have same functionality
  ;if LHS output == RHS output, return values into output file
  (define inlst (file->list input))
  (define outlst (twolst inlst))

  (define outfile (open-output-file #:exists 'replace output))
  
  (write-to-file outfile inlst outlst (length outlst))
  
  (close-output-port outfile)
    ;Outputs with two columns, input numbers on the left, output numbers on the right,
  ;Only one output column is necessary since two-LHS and RHS have the same functionality
)

;expects input and output files in the format "input.txt" and "output.txt"
;Input file has numbers in a sequence like "1 2 3 4 5"
(define (sum-squares input output)
  
  (define inlst (file->list input))
  (define outlst (threelst inlst))

  (define outfile (open-output-file #:exists 'replace output))
  
  (write-to-file outfile inlst outlst (length outlst))
  
  (close-output-port outfile)

)

(define (write-to-file outfile inlst outlst lstlength)
  (cond
    [(null? inlst) '()]
    [(not (null? inlst)) (write-line outfile (car inlst) (car outlst) lstlength) (write-to-file outfile (cdr inlst) (cdr outlst) lstlength)]
  )
)

(define (write-line outfile invalue outvalue lstlength)
  (fprintf outfile "~a     ~a~n" invalue outvalue)
)

(define (twolst inputlst)
  (cond
    [(equal? (map (lambda (x) (two-LHS x)) inputlst) (map (lambda (x) (two-RHS x)) inputlst)) (map (lambda (x) (two-RHS x)) inputlst)]
  )
)

(define (threelst inputlst)
  (cond
    [(equal? (map (lambda (x) (three-LHS x)) inputlst) (map (lambda (x) (three-RHS x)) inputlst)) (map (lambda (x) (three-LHS x)) inputlst)]
  )
)

(define (symdiff-file infile outfile)
  (define in (open-input-file infile))

  ;Expecting list of form (1 2 3) for input
  (define inlst1 (read in))
  (define inlst2 (read in))

  ;(display inlst1) (newline)
  ;(display inlst2) (newline)

  (define outlst (symmetric-difference (remove-duplicates inlst1) (remove-duplicates inlst2)))

  (define out (open-output-file outfile #:exists 'replace))
  (write outlst out)
  (close-output-port out)
)

;;;;;;;;;;; END LAB 3 ;;;;;;;;;;;;

;;;;;;;;;;; Begin test functions ;;;;;;;;;;

(define (testlab3)
  (sum-up "in.txt" "out.txt")
  (sum-squares "in.txt" "out.txt")
  (symdiff-file "inlst.txt" "outlst.txt")
)

(define (testlab2)
  ;test symmetricDifference
  (define a '(1 2 3 4))
  (define b '(3 4 5 6))
  (define c '(once upon a time mary had))
  (define d '(time mary had a little lamb))
  ;final set of a and b should be '(1 2 5 6)
  ;final set of c and d should be '(once upon little lamb)
  (display "symmetric-difference") (newline)
  (display (symmetric-difference a b)) (newline)
  (display (symmetric-difference c d)) (newline) (newline)

  ;test two-LHS and two-RHS
  (display "two-LHS") (newline)
  
  (display (two-LHS 1)) (newline)
  (display (two-LHS 3)) (newline)
  (display (two-LHS 5)) (newline)
  (display (two-LHS 7)) (newline) (newline)

  (display "two-RHS") (newline)
   
  (display (two-RHS 1)) (newline)
  (display (two-RHS 3)) (newline)
  (display (two-RHS 5)) (newline)
  (display (two-RHS 7)) (newline) (newline)

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
  



