#lang racket

;Andrew Nguyen
;W 0118 3647
;CSCI 301 Summer 2017

(define (C x)
  (cond
    [(or (= x 1) (= x 2)) 1]
    [else (C (+ (C (- x 1)) (C (- x (C (- x 1))))))]
  )
)

(define (A x)
  (cond
    [(or (= x 1) (= x 2)) 1]
    [else (+ (A (A (- x 1))) (A (- x (A (- x 1)))))]
  )
)

;must account for the case of nested sets
;thus in the case of '(a (b (c d)) ((d c) b))
;and '((b (c d)) ((d c) b)), we must confirm that the individual elements exist on both sides
(define (set-equal? s1 s2)
  ;two cases
  ;if none of the elements in s1 and s2 are lists, then use simple subset equality
  ;if they both contain list elements, ie s1 = (b (c d)) and s2 = ((d c) b), must use recursive calls
  (cond
    [(and (no-nested-elements? s1) (no-nested-elements? s2)) (and (subset? s1 s2) (subset? s2 s1))]
    [else (handle-nested-sets s1 s2)]
  )
)

;(handle-nested-sets '(a (b (c d))) '(((d c) b) a))
(define (handle-nested-sets s1 s2)
  ;(display "handled") (newline)
  (cond
    ;if it even got to this function, no-nested-elements? is false for at least s1 or s2.
    ;must check another layer in. (ene s1) gives a list of only ((x) (y) (z)), where x,y,z are all nested list elements
    ;ie: x,y,z = (c d) or (b (c d)). So they can also be further nested sets. This is what I mean by checking another layer in.
    ;I should make this recursive until null '(), because (ene s1) returns null if there are no more nested sets.
    ;For example, (ene s1) the first time would return ((a b) (c d)), check for existence in the opposing set,
    ;and then recursively execute on all elements of the list, so (ene '(a b)) and (ene '(c d)), both would return '()

    ;recursion implemented via another execution of set-equal on subsets of the first input sets. Test using (testsets)
     
    [(and (and (not (null? s1)) (not (null? s2))) (and (no-nested-elements? (ene s1)) (no-nested-elements? (ene s2))))
     (and (compare-single-elements (ese s1) (ese s2)) (compare-nested-elements (ene s1) (ene s2)))]
    [#t (and (compare-single-elements (ese s1) (ese s2)) (or (compare-nested-elements (ene s1) (ene s2))
                                                             (and (handle-nested-sets (car (ene s1)) (car (ene s2))) (set-equal? (cdr (ene s1)) (cdr (ene s2))))))]
  )
)

;is the set s1 devoid of nested elements/sets in it?
;'(a b c) returns true, '(a (b c)) returns false
(define (no-nested-elements? s1)
  (cond
    [(not (null? s1)) (and (not (list? (car s1))) (no-nested-elements? (cdr s1)))]
    [(null? s1) #t]
  )
)

;stands for "extract single elements." Helper function to take the single elements of a mixed list
;and put them in their own list for individual processing.
;example, the list '(b (c d)) would extract only b and output '(b)
(define (ese s1)
  (cond
    [(and (not (null? s1)) (not (list? (car s1)))) (append (list (car s1)) (ese (cdr s1)))]
    [(and (not (null? s1)) (list? (car s1))) (ese (cdr s1))]
    [(null? s1) null]
  )
)

;"extract nested elements," same functionality as ese, but extracts nested elements
;input '(b (c d)) outputs '((c d)), input '(a (b (c d))) outputs '(b (c d)).
;infinitely many nested sets are handled by parent function recursion
(define (ene s1)
  (cond
    [(and (not (null? s1)) (list? (car s1))) (append (list (car s1)) (ene (cdr s1)))]
    [(and (not (null? s1)) (not (list? (car s1)))) (ene (cdr s1))]
    [(null? s1) null]
  )
)

;suppose there is a master set like '(a (b (c d)))
;compare-single-elements is fed the single elements of a SINGLE execution of (ese) on s1 and s2.
;So (ese '(a (b (c d)))) = '(a). These single elements are matched to other single elements in the other set
(define (compare-single-elements s1 s2)
  (and (subset? s1 s2) (subset? s2 s1))
)

;Again, if the master set is '(a (b (c d))), then (ene '(a (b (c d)))) = '(b (c d)). But 'b is a single element
;This is taken care of by higher level function recursion. Specifically recursion in handle-nested-sets and set-equal?
;so only '((c d)) would be fed into compare-nested-elements
(define (compare-nested-elements s1 s2)
  ;s1 and s2 ares sets like
  ;'((c d) (a b)) and '((b a) (d c))
  (cond
    [(and (not (null? s1)) (not (null? s2))) (and (find-match (car s1) s2) (compare-nested-elements (cdr s1) s2))]
    [(null? s1) #t]
    [else #f]
  )
)

;helper function for compare-nested-elements.
;compare-nested-elements supplies a single non-nested set for s1, and the full opposing set for s2
;so s1 could be '(c d) and s2 would be '((c d) (e f)). Determines if s1 is in s2. ie: is there a match for s1?
;s2 is always a set of nested sets with no single elements
(define (find-match s1 s2)
  (cond
    [(not (null? s2)) (or (mutual-subset? s1 (car s2)) (find-match s1 (cdr s2)))]
    [(null? s2) #f]
  )
)

;essentially is s1 ≥ s2 and s1 ≤ s2? ie: s1 = s2?
(define (mutual-subset? s1 s2)
  (and (subset? s1 s2) (subset? s2 s1))
)

;is s1 ≥ s2 ?
(define (subset? s1 s2)
  (cond
    [(not (null? s1)) (and (contains (car s1) s2) (subset? (cdr s1) s2))]
    [(null? s1) #t]
  )
)

;is element x in set lst? return true/false
(define (contains x lst)
  (cond [(member x lst) #t]
        [else #f]
   )
)

(define (testset)
  ;test function for set-equal? predicate

  ;typical single element sets
  (define s1 '(a b c d e f))
  (define s2 '(a b c d e))
  (display (set-equal? s1 s2)) (newline) ;#f

  (set! s1 '(a b c d e))
  (set! s2 '(a b c d e))
  (display (set-equal? s1 s2)) (newline) ;#t

  (set! s1 '(e d c b a))
  (set! s2 '(a b c d e))
  (display (set-equal? s1 s2)) (newline) ;#t

  (set! s1 '(a b c d e))
  (set! s2 '(a b c d e f))
  (display (set-equal? s1 s2)) (newline) ;#f

  ;beginning of nested sets, 1 layer of nesting
  (set! s1 '((a b) (c d) (e f)))
  (set! s2 '(a b c d e f))
  (display (set-equal? s1 s2)) (newline) ;#f

  ;three nested sets in a single set
  (set! s1 '((a b) (c d) (e f)))
  (set! s2 '((a b) (c d) (e f)))
  (display (set-equal? s1 s2)) (newline) ;#t

  ;Spec sheet examples
  (set! s1 '(a (b (c d)))) ;(x (y (z)))
  (set! s2 '((d c b) a)) ;((x) y)
  (display (set-equal? s1 s2)) (newline) ;#f

  (set! s1 '(a (b (c d))))
  (set! s2 '(((d c) b) a))
  (display (set-equal? s1 s2)) (newline) ;#t

  (set! s1 '((b (c d))))
  (set! s2 '(((d c) b)))
  (display (set-equal? s1 s2)) (newline) ;#t

  ;final tests. triple nested sets
  (set! s1 '((((e d) c) b) a))
  (set! s2 '(a (b (c (d e)))))
  (display (set-equal? s1 s2)) (newline) ;#t

  (set! s1 '(((a) (b)) (c (d)) (e f)))
  (set! s2 '(((a) (b)) (c d) (e f)))
  (display (set-equal? s1 s2)) (newline) ;#f

  (set! s1 '(a (b (1) c) (d e) f))
  (set! s2 '(f (e d) (c (1) b) a))
  (display (set-equal? s1 s2)) (newline) ;#t
)

(define (testall)
  (display (C 1)) (newline)
  (display (C 2)) (newline)
  (display (C 3)) (newline)
  (display (C 4)) (newline)
  (display (C 5)) (newline)
  (display (C 6)) (newline)
  (display (C 7)) (newline)
  (display (C 8)) (newline)
  (display (C 9)) (newline)
  (display (C 10)) (newline) (newline)

  (display (A 1)) (newline)
  (display (A 2)) (newline)
  (display (A 3)) (newline)
  (display (A 4)) (newline)
  (display (A 5)) (newline)
  (display (A 6)) (newline)
  (display (A 7)) (newline)
  (display (A 8)) (newline)
  (display (A 9)) (newline)
  (display (A 10)) (newline) (newline)

  (testset)
)