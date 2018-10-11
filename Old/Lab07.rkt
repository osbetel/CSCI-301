#lang racket

(define tokens '(thousand million billion trillion quadrillion quintillion sextillion septillion octillion nonillion decillion
                          undecillion duodecillion tredecillion quattuor-decillion quindeccillion sexdecillion septen-decillion
                          octodecillion novemdecillion vigintillion)) ;goes up to 10^63

(define (rmle lst)
    (if (null? (cdr lst))
        '()
        (cons (car lst) (rmle (cdr lst)))
    )
)

(define (rm3 lst)
  (rmle (rmle (rmle lst)))
)

(define (rm2 lst)
  (rmle (rmle lst))
)
                           

;function to convert numbers into a list: eg 582 -> '(5 8 2)
(define (int-list num)
  (if (zero? num)
      `()    ;Return an empty list if zero since (number-name 0) == '()
       (append (int-list (quotient num 10)) (list (remainder num 10))) ;Just like a division and modulus function
  )
)

;takes list '(1 2 3 4 5 6) and changes it to '(123 456)
(define (int-list-by3 numlst len)
  (cond [(>= len 3) (list (rm3 numlst) (by3-helper numlst len))])
)

(define (by3-helper numlst len)
  (define x (number->string (list-ref numlst (- len 3))))
  (define y (number->string (list-ref numlst (- len 2))))
  (define z (number->string (list-ref numlst (- len 1))))
  (cond [(and (= (string->number x) 0) (= (string->number y) 0) (= (string->number z) 0)) (list "000")]
        [else (list (string->number(string-append (string-append x y) z)))]
  )
)

(define (processlst numlst)
  ;(display numlst) (newline)
  (cond [(list? numlst) (int-list-by3 numlst (length numlst))]
        [else numlst])
)

(define (manage twinlst times)
  (cond [(list? twinlst)
  (define x (list-ref twinlst 0))
  (define y (list-ref twinlst 1))
  (define pplst (processlst x))
  ;(display pplst) (newline)

  (cond
    [(not (= times 0)) (append (manage (list (list-ref pplst 0) (list-ref pplst 1)) (- times 1)) (list-ref pplst 1))]
    [else x]
  )]
  [else twinlst])
  ;(append (car (process numlst)) (list-ref (process numlst) (- (length (process numlst)) 1)))
)

;deletes first element in the list that matches the symbol
(define delete (lambda (item numlst)
     (cond
       [(equal? item (car numlst)) (cdr numlst)]
       [else (cons (car numlst) (delete item (cdr numlst)))]
     )
   )
)

;recursively removes double words. Used in tripleZero-rm
(define (doubleWords? numlst)
  (if (not (null? (cdr numlst)))
      (cond
      [(and (symbol? (car numlst)) (symbol? (car (cdr numlst)))) (delete (car (cdr numlst)) numlst)]
      [else (cons (car numlst) (doubleWords? (cdr numlst)))]
      )
      numlst
  )
)

;determines if element x is in the list
(define (contains lst x)
    (cond 
        [(null? list) #f]
        [(eq? (car list) x) #t]
        [else (contains (cdr list) x)]
    )
)

;removes any occurrence of 000 and the token directly after
(define (tripleZero-rm numlst)
  (define lst (doubleWords? (delete "000" numlst)))
  (if (contains lst "000")
      (tripleZero-rm lst)
      lst
  )   
)

;;;;;;;;;;;;;;DEPRECATED;;;;;;;;;;;;;;
(define (clean-list numlst)
  ;is there 1 or 2 values at the front?
  (cond [(< (second numlst) 10) (combine-first-two numlst)]
        [else numlst])
)
;;;;;;;;;;;;;;DEPRECATED;;;;;;;;;;;;;;

;;;;;;;;;;;;;;DEPRECATED;;;;;;;;;;;;;;
(define (combine-first-two numlst)
  (define x (string->number (string-append (number->string (first numlst)) (number->string(second numlst)))))
  (append (list x) (cdr (cdr numlst)))
)
;;;;;;;;;;;;;;DEPRECATED;;;;;;;;;;;;;;
          

(define (number-name-calc num)
  ;convert to a list
  (define numlst (int-list num))
  (define result '())
  (define times (quotient (length numlst) 3))
  (define twinlst (list numlst result))
  ;(clean-list (manage twinlst times))
  (manage twinlst times)
 
  
  ;while len > 0, add numbers in groups of 3 to the resultlst, from left-to-right, and decrement ln
  ;eg, if len = 9, then, the number is in the form xxx,xxx,xxx. So, that would be '(xxx million xxx thousand xxx)
  ;so len/3 gives 9/3 = 3, which means we can use this as 3 = millions place, 3 - 1 = 2 = thousands place, 3 - 2 = 1
  ; = ones place
)

;insert x at index in lst
(define (list-insert x index lst)
  (if (= index 0)
      (cons x lst)
      (cons (first lst) (list-insert x (- index 1) (rest lst)))
  )
)

;Recursively inserts tokens for thousands, millions, billions, etc.
(define (insert-tokens numlst places insert)
  (if (not (= places -1)) ;if places = 2, we 
      (insert-tokens (list-insert (list-ref tokens places) insert numlst) (- places 1) (+ insert 2))
      numlst
  )
)

;produces a list from a number, ie 123456 --> 123 thousand 456
(define (number-name num)
  (define lst (number-name-calc num))
  (define places (- (length lst) 2))
  (set! lst (insert-tokens lst places 1))
  (cond
    [(contains lst "000") (tripleZero-rm lst)]
    [else lst])
  
)

;Definitions for name<1000
(define singles '(zero one two three four five six seven eight nine))
(define teens '(ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen))
(define tens '(twenty thirty forty fifty sixty seventy eighty ninety))
;no need to define hundreds

;Converts a given list of numbers ie '(4 5 6) into four hundred fifty six
(define (name<1000 numlst)
  ;loop through
  ;(real? (car lst)) --> convert num to word, return word
  (define len (length numlst))
  
  (cond
    [(= len 1) (list (list-ref singles (first numlst)))]; 1 - 9
    [(and (= len 2) (= (car numlst) 1)) (list (list-ref teens (second numlst)))]; 10 - 19
    [(and (= len 2) (> (car numlst) 1) (not (= (second numlst) 0))) (list (list-ref tens (- (first numlst) 2)) (list-ref singles (second numlst)))]; 20 - 99
    [(and (= len 2) (> (car numlst) 1) (= (second numlst) 0)) (list (list-ref tens (- (first numlst) 2)))]
    [(and (= len 2) (= (car numlst) 0)) (list (list-ref singles (second numlst)))]
    [(= len 3) (append (list (list-ref singles (first numlst)) 'hundred) (name<1000 (cdr numlst)))]
  )
)

;Recursive kicker function for name<1000
(define (preNums->words numlst)
  (if (not (null? (cdr numlst)))
      (cond
      [(real? (car numlst)) (append (name<1000 (int-list (car numlst))) (cdr numlst))]
      [else (cons (car numlst) (preNums->words (cdr numlst)))]
      )
      numlst
  )
)

(define (allSymbols->nonReal numlst flag)
  (define lst (preNums->words numlst))
  (if (> flag 0)
      (allSymbols->nonReal lst (- flag 1))
      lst
  )   
)

(define (finalNumberConverter num)
  (define lst (number-name num))
  (allSymbols->nonReal lst (+ 1(length lst)))
)

(define (t)
  (display (finalNumberConverter 123000456000)) (newline)
  (display (finalNumberConverter 1456000)) (newline)
  (display (finalNumberConverter 1000000)) (newline)
)

