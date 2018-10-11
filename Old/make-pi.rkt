#lang racket

;Andrew Nguyen, CSCI 301 @ Western Washington University
;Professor Geof Matthews, Lab Assgn. #1
;January 13, 2017

(define (make-pi accu)
  ;must compute 4/1 - 4/3 + 4/5 ... until change is less than accu
  ;need a var for numerator (4) and var for denominator (1 + nLoops)
  ;Also need a var for -1 to change the sign with each loop

  (pi-loop accu 0.0 4.0 1.0 0) ;1st time sets pi = 4.0
)  

(define (pi-loop accu pi nume deno iterations)
  (if (> (/ nume deno) accu)
      ;if block
      ;the begin statement is required if executing multiple statements
      ;(or procedures as scheme calls them) in a conditional statement block
      (begin
       (set! pi (+ pi (* (expt -1 iterations) (/ nume deno)))) ;[(-1)^iterations * nume/deno]

       ;***Uncomment the below line to display all the calculations of pi with each loop***
       (display pi) (newline)
       
       (set! deno (+ deno 2)) ;iterate the denominator by 2
       (set! iterations (+ iterations 1)) ;iterate the number of tracked loops by 1
       
       (pi-loop accu pi nume deno iterations) ;recursive call
       )

      ;else block – no idea how to get around this but scheme doesn't allow for an if conditional statement
      ;that only has one conditional block. It must have an else block that acts as a procedure. Consider
      ;looking into the cond statement for this kind of problem next time. 
      (+ pi 0)  ;still returns the most recent pi though
  )
)

(define (cont-frac n d k)
    (define (loop-frac n d i) ;need some way to loop from start to k times, here i starts as 0

        ;if we hit the end (k), then it is nk / dk
        (if (= i k)
            ; base case;
            (d i)
            
            ;...otherwise, iterate and return the next continued fraction
             (+ (d i)
                (/ (n (+ i 1))
                   (iter-frac n d (+ i 1))
             )) ;This part really bugged me until I figured out the inputs would be in the form of functions
                ;eg: inputting 1.0 would be done as lambda(x) 1.0. Then I realized each of these variables needed
                ;a parameter variable as well. "d of i," "n of i + 1," and so on.
        )
    )
    (/ (n 1) (loop-frac n d 1)) ;Start with n1 as the numerator, and the recursive index at 1 for the denominator
)

(define (euler-d i)return a number, d of index i in the pattern 1, 2, 1, 1, 4, 1, 1, 8...
    (define (eOfI i)
        ;every multiple of 3, - 1, is a non-1 index value.
        ;eg, index 3*1-1 = index 2, which has a value of 2
        ;index 3*4-1 = index 11, which has a value of 8.
        ;Another way to think of this is e(i) = (i+1)/3*2, where i
        ;represents an eligible index. e(2) = (2+1)/3*2 = 2.
        ;e(14) = (14+1)/3*2 = 10

        (* (/ (+ i 1) 3) 2)
    )
    (let ([x (eOfI i)])
        (if (integer? x) ;x will only ever be an integer if the index+1 is perfectly divisible by 3
            (+ x 0)
            (+ 1 0)
            ;otherwise if i ≠ 1
        )
    )
)

;In the next two functions, note the use of float numbers.
;Required otherwise shceme returns a ridiculously ridiculous fraction that is unreadable.
(define (make-lambert-n x)
    (lambda(i) (if (= i 1)
                    (+ x 0)
                    (* -1.0 x x)
                )
      )
)

(define (lambert-d i) ;return numbers according to index: 1, 3, 5, 7, 9, 11...
    (if (= i 1)
        (+ 1.0 0)
        (- (* 2.0 i) 1)
    )
)


