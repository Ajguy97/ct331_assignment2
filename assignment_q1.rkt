#lang racket

(cons 1 2)
(cons 1 (cons 2 (cons 3 '())))
(cons "String" (cons 1 (cons (cons 1 (cons 2 (cons 3 '()))) '())))
(list "String" 1 '(1 2 3))
(append '("String") '(1) '((1 2 3)))

;list is just a function that uses the cons function and makes it easier to
;create lists rather than calling cons each time
;append can only take lists as arguments, can't take atoms
