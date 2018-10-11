#lang racket
;Andrew Nguyen
;W 0118 3647
;CSCI 301 Summer 2017

;Define list of boolean constants
;eg: constants = '(and not or implies if iff)
;loop through all elements of the input list, if (x1 not in constants, add to output list)

(define boolean-constants '(and not or implies if iff))

(define (collect-prop-variables l1)
  (if (not (no-nested-elements? (extract-vars l1)))
      (remove-duplicates (append (ese (extract-vars l1)) (collect-prop-variables (car (ene l1)))))
      (remove-duplicates (extract-vars l1)) ;removes duplicate variables
  )
)

(define (replace-instance l1 x y)
  (cond ((pair? l1)
         (let ((next (replace-instance (car l1) x y)))
           (cons next 
                 (if (equal? next (car l1))            ; changed?
                     (replace-instance (cdr l1) x y)    ;   no,  recurse on rest
                     (cdr l1)))))                      ;   yes, done
         ((eq? l1 x) y)
         (else l1)))

(define (contains-nested? l1 x)
  (cond
    [(empty? l1) #f]
    [(equal? (car l1) x) #t]    
    [(list? (car l1)) (or (contains-nested? (car l1) x) (contains-nested? (cdr l1) x))]
    [else (contains-nested? (cdr l1) x)]
  )
)

;replace all x with y in list l1
(define (substitute l1 x y)
  (cond
    [(contains-nested? l1 x) (substitute (replace-instance l1 x y) x y)]
    [else l1]
  )
)

(define (Evaluate-WFF expr)
  (cond
    [(= (length expr) 1) (car expr)]
    [(equal? 'not (car expr)) (not (Evaluate-WFF (cdr expr)))]
    [else (eval (cdr expr) (car expr))]
   )
)

(define (eval expr pre)
  ;(display expr) (newline)
  (cond
    [(list? (car expr)) (Evaluate-WFF (car expr))]
    [else (match (car expr)
            ['not (not (eval (cdr expr) (car expr)))] ;Works
            ['or (or pre (eval (cdr expr) (car expr)))] ;works
            ['and (and pre (eval (cdr expr) (car expr)))] ;works
            ['iff (equal? pre (Evaluate-WFF (cdr expr)))] ;works
            ['implies (or (equal? pre (Evaluate-WFF (cdr expr))) (equal? #t (Evaluate-WFF (cdr expr))))] ;works
            ['#t #t]
            ['#f #f]
            ;['() #t]
          )]
  )
)

(define (extract-vars l1)
  (cond
    [(not (null? l1)) (if (not (contains (car l1) boolean-constants))
                          (append (list (car l1)) (collect-prop-variables (cdr l1)))
                          (collect-prop-variables (cdr l1))
                       )]
    [(null? l1) '()]
  )
)

(define (ese s1)
  (cond
    [(and (not (null? s1)) (not (list? (car s1)))) (append (list (car s1)) (ese (cdr s1)))]
    [(and (not (null? s1)) (list? (car s1))) (ese (cdr s1))]
    [(null? s1) null]
  )
)

(define (ene s1)
  (cond
    [(and (not (null? s1)) (list? (car s1))) (append (list (car s1)) (ene (cdr s1)))]
    [(and (not (null? s1)) (not (list? (car s1)))) (ene (cdr s1))]
    [(null? s1) null]
  )
)

(define (contains x lst)
  (cond [(member x lst) #t]
        [else #f]
   )
)

(define (no-nested-elements? s1)
  (cond
    [(not (null? s1)) (and (not (list? (car s1))) (no-nested-elements? (cdr s1)))]
    [(null? s1) #t]
  )
)

(define (testall)
  ;not
  (display (Evaluate-WFF '(not #t))) (newline) ;#f
  (display (Evaluate-WFF '(not #f))) (newline) ;#t
  (newline)

  ;or
  (display (Evaluate-WFF '(#t or #t))) (newline) ;#t
  (display (Evaluate-WFF '(#t or #f))) (newline) ;#t
  (display (Evaluate-WFF '(#f or #f))) (newline) ;#f
  (display (Evaluate-WFF '(#f or #t))) (newline) ;#t
  (newline)

  ;and
  (display (Evaluate-WFF '(#t and #t))) (newline) ;#t
  (display (Evaluate-WFF '(#t and #f))) (newline) ;#f
  (display (Evaluate-WFF '(#f and #f))) (newline) ;#f
  (display (Evaluate-WFF '(#f and #t))) (newline) ;#f
  (newline)

  ;iff
  (display (Evaluate-WFF '(#t iff #t))) (newline) ;#t
  (display (Evaluate-WFF '(#t iff #f))) (newline) ;#f
  (display (Evaluate-WFF '(#f iff #f))) (newline) ;#t
  (display (Evaluate-WFF '(#f iff #t))) (newline) ;#f
  (newline)
  
  ;implies
  (display (Evaluate-WFF '(#t implies #t))) (newline) ;#t
  (display (Evaluate-WFF '(#t implies #f))) (newline) ;#f
  (display (Evaluate-WFF '(#f implies #f))) (newline) ;#t
  (display (Evaluate-WFF '(#f implies #t))) (newline) ;#t
  (newline)

  ;complex statements
  (display (Evaluate-WFF '(#t or (not #t)))) (newline) ;#t
  (display (Evaluate-WFF '(#t and (not #t)))) (newline) ;#f
  (display (Evaluate-WFF '(#t and (#t or (not #t))))) (newline) ;#t
  (display (Evaluate-WFF '(#t iff (not #t)))) (newline) ;#f
)