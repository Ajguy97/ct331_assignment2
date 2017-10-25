#lang racket

(define (sub_tree middle left right)
  (cond
    [(equal? middle '()) (display "Error: root can't be empty")]
    [(equal? middle list left)(display "Error: No duplicates")]
    [(equal? middle right)(display "Error: No duplicates")]
    [(equal? right left)(display "Error: No duplicates")]
    [else (list middle left right)]
  ))



(define (rootNode node)
  (car node))

(define (leftNode node)
   (car node))

(define (rightNode node)
  (caddr node))


(sub_tree '() 1 2)






