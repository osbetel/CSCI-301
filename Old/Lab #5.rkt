#lang racket

; Lab #5, CSCI 301, Prof. Matthews
; Andrew Nguyen, W01183647

; To convert a string into pig latin, we must first produce a list form of the string, like java where it is "string here".split(" ").
; Splitting the string by spaces gives an array (or a list in Scheme) ["string", "here"] and then we produce a loop
; The loop then processes every element of the array individually
; Also need a way to determine the first character of a word to see if it is a vowel.
; If vowel, then simply concat a "way" to the end of the word.
; In order to convert a string to its pig latin form, I'll have to convert a string into a list of characters and move them around that way


;We don't typically use global variables, they're used in this one case to avoid
;typing out these lists over and over.
;None of the actual algorithm work uses global variables to save lists and such
(define way '(#\w #\a #\y)) ;if word begins with vowel
(define ay '(#\a #\y)) ;if word begins with consonant
(define vowels '(#\a #\e #\i #\o #\u)) ;used to check if a letter is in this list of vowels

;main function, in the form (piglatin "string"). Every other function exists to support this one
(define (piglatin str)
  (define lst (string-split str));Takes the string and makes it a list: '(this is a list)
  (set! lst (map (pigify) lst)) ;map applies pigify function to all elements
  (string-join lst)) ;joins the list of modified words back into a string

;pigifies a single word
(define (pigify)
  (lambda (str) ;note, must be lambda because map uses the form (map <func> <list>). <func> accepts parameters but we want the list's elements as parameters
    (set! str (string->list str)) ;converts the "word" into (w o r d)
    (convert-word str))
)

;called by pigify
(define (convert-word l2)

  ;determine if the first letter is a vowel
  (define boolean (isVowel (car l2)))
  (if boolean
      (set! l2 (pigify-v l2))
      (set! l2 (append (pigify-c l2) ay))
  )
  ;returning l2's back as a string eg: (a p p l e) --> appleway
  (list->string l2)
)

;checks if str is in '(a e i o u), returns #t or #f
(define (isVowel str)
  (define boolean (member str vowels))
  (not (boolean? boolean))
)

;If the word stays with a consonant, its an easy conversion, append "way" to the end
(define (pigify-v l3)
  (append l3 way)
)
  
(define (pigify-c l3)
  ;l3 is a list like '(s p o r k)
  ;Must consume and store up to the first vowel
  ;temp = '(s p), then (append '(s p) '(a y)) to get '(s p a y)
  ;finally, apppend the ork and the spay to get '(o r k s p a y)

  ;Only reaches pigify-c if the first character is a consonant
  ;l3 = (h e l l o), then l4 = (h) and l5 = (e l l o)
  (define l4 (list (car l3)))
  (define l5 (cdr l3))
  ;(display l4) (newline)
  ;(display l5) (newline)

  ;check if cdr l3 is null or not, ie: if there are no more characters
  (define canContinue (not(null? l5)))

  ;if passes first check, check if the next letter in the word is a vowel or consonant. If consonant, continue
  (set! canContinue (and canContinue (isVowel (car l5))))

  ;(display l4) (newline)
  ;(display l5) (newline)
  
  (display canContinue) (display " ") (display l4) (display " : ") (display l5) (newline)
  ;if passes the above two checks, ie: #f if there are still more characters to process and the next character is not a vowel
  (if (equal? #t canContinue)
      (append l5 l4)
      (append (pigify-c l5) (append l4 (list (car l5))))
  )
)

;runs test
(define (t)
  ;(display (piglatin "the quick brown fox jumps over the lazy dog"))
  
  (display (piglatin "hello there you gorgeous thing are you busy tonight")) (newline)
  ;sample "ellohay erethay ouyay orgeousgay ingthay areway ouway usybay onighttay"
  ;my output "ellohay thereay uoyay orgeousgay thingay areway uoyay usybay onighttay"
  
  ;(display (piglatin "hello") (newline)) ;test 1 consonant at the start of the word
  ;(display (piglatin "spork")) (newline) ;2 consonant test
)