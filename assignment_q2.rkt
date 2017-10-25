#lang racket

;This is an example implementation of ins_beg,
;It obviously doesn't do what it should, so you
;can edit this function to get started.
;
;Please note the provide function is necessary
;for the unit tests to work. Please include a
;(provide) for each function you write in your
;submitted assignment.
;
;You may delete these comments!

(provide ins_beg)
(provide ins_end)
(provide cout_top_level)
(provide count_instances)
(provide count_instances_tr)
(provide count_instances_deep)


(define (ins_beg el lst)
  (append (list el) lst)
  )
  
(define (ins_end el lst)
  (append lst (list el))
  )

(define (cout_top_level lst)
  (length lst)
  )

;;Tail recursive
(define (count_instances_tr el lst)
  (cond
    [(null? lst) 0] ;if list is empty then return is 0
    [(= el (car lst)) (+ 1 (count_instances_tr el (cdr lst)))];if el is equal to first item in list add 1 to the count_instances of cdr
    ;else add nothing
    [else (count_instances_tr el(cdr lst)) ]
  ))

;;non-Tail recursive
(define (count_instances el lst)
  (c_instances el lst 0)
  )

(define (c_instances el lst total)
  (cond
  [(null? lst) total]
  [(= el (car lst)) (c_instances el (cdr lst) (+ 1 total))]
  [else (c_instances el (cdr lst) total)]
  ))

(define (count_instances_deep el lst)
  (c_instances_deep el lst 0)
  )

(define (c_instances_deep el lst total)
  (cond
    [(null? lst) total] ;if list is empty then return is 0
    ;;if first element is a list do count instances in that
    [(list? (car lst))(c_instances_deep el (cdr lst) (c_instances el (car lst) total))]
    [(= el (car lst)) (c_instances_deep el (cdr lst) (+ 1 total))]
    [else (c_instances_deep el(cdr lst) total) ]
  ))

(ins_beg 'a '(b c d))
(ins_beg '(a b) '(b c d))
(ins_end 'a '(b c d))
(ins_end '(a b) '(b c d))
(cout_top_level '(1 2 3 (4 5)))
(count_instances_tr 1 '(1 1 2 3 1))
(count_instances 1 '(1 1 2 3 1))
(count_instances_deep 1 '(1 2 3 (1 1)) )



