#lang racket

;Andrew Nguyen, CSCI 301 @ Western Washington University
;Professor Geof Matthews, Lab Assgn. #4
;February 7, 2017

(define (compose f g)
  ;(lambda (x) ((f) (lambda (x) ((g) x))))
  (lambda (x) ((f) ((g) x)))
)

(define (square) (lambda (x) (* x x)))
(define (add1) (lambda (x) (+ x 1)))
(define (cube) (lambda (x) (* x x x)))
;sqrt, tan, sin, cos are also all integrated functions
(define (linear) (lambda (x) (+ (* -4 x) 13))) ;-4x + 13
(define (quadratic) (lambda (x) (+ (* 3 (* x x)) (* -4 x) 5)))

;((repeated-r square 2) 5) would apply the square function to 5, 2 times. In essence, (5^2)^2 = 625
;I debated with myself for a while and I think this is a recursive function and not a tail-recursive, even if the
;last line(s) are a recursive call. Primarily because the calculation requires the bottom recursive call to
;be active first. ie: there is no calculation before making the recursive call, but there is calculation after.
;((repeated-r f(x) n) x) requires a call of ((repeated-r f(x) (n-1)) x) before it applies calculations.
;PRECONDITION: N MUST BE 1 OR GREATER. If is less than 1 it only calculates 1 iteration of f(x)
(define (repeated-r f n)
  (if (> n 1)
      (lambda (x) ((f) ((repeated-r f (- n 1)) x)))
      (lambda (x) ((f) x))
      ;recursive because each iteration creates another "f(x) around the first one.
      ;at the bottom-most function call, it might look like (f(f(f(x))) whereas a tail-recursive function
      ;would simply look like (f(previous_result)), thus running in O(1) space (but not time).
      ;This O(1) space complexity is only possible if a compiler supports tail-recursion though. I know C does.
  )
)

;This is true tail recursive since the call appears at the end, and the calculation happens before the function is called.
;The (+ 0 x) is returned as the base-case when all iterations are completed. Again, (+ 0 x) because scheme does not allow
;for returning a plain old x.
;PRECONDITION: n must be at least 1 or greater to have any effect. If it is less than 1 it simply returns the number x
(define (repeated-i f n)
  (lambda (x)
    (if (> n 0)
        ((repeated-i f (- n 1)) ((f) x))
        (+ 0 x)
        ;This is tail-recursive because at the end of all iterations, the result has been calculated fully
        ;So then (+ 0 x) can just return (in C->assembly this is the jump call)
        ;to the top of the stack (if the scheme compiler supports this)
    )
  )
)

(define (testAll)
  (display "repeated recursive tests")
  (newline)
  
  (display ((repeated-r linear 4) 7))
  (newline)
  
  (display ((repeated-r square 2) 4))
  (newline)
  
  (display ((repeated-r quadratic 1) 19))
  (newline)
  
  (display ((repeated-r add1 214) -4))
  (newline)

  (newline)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (display "repeated iterative tests")
  (newline)

  (display ((repeated-i linear 4) 7))
  (newline)
  
  (display ((repeated-i square 2) 4))
  (newline)
  
  (display ((repeated-i quadratic 1) 19))
  (newline)
  
  (display ((repeated-i add1 214) -4))
  (newline)

  (newline)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (display "compose function tests")
  (newline)

  (display ((compose square add1) 5))
  (newline)
  
  (display ((compose square linear) 6))
  (newline)
  
  (display ((compose quadratic linear) -7))
  (newline)
  
  (display ((compose add1 cube) 3))
  (newline)
)


