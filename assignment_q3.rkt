#lang racket

;;found it easier to work with middle at the start
(define (node middle left right)
  (list middle left right)
  )

(define (root node)
  (car node)
  )

(define (left_subtree node)
  (cadr node)
  )

(define (right_subtree node)
  (caddr node)
  )
;A
(define (traverse tree)
  (if(null? tree)
  '()
  (begin
    (traverse (left_subtree tree))
    (display (root tree))(newline)
    (traverse (right_subtree tree))
  )))
;B
(define (search x tree)
  (if (null? tree)
  #f
  (let ((current (root tree)))
    (cond
      [(< x current)(search x (left_subtree tree))]
      [(> x current)(search x (right_subtree tree))]
      [(equal? x current) #t ]
     )
  )))



;;C
(define (insert x tree)
  (if (null? tree)
   (node x '() '())
   (let ((current (root tree)))
     (cond
      [(< x current)(node current (insert x (left_subtree tree)) (right_subtree tree) )]
      [(> x current)(node current (left_subtree tree) (insert x (right_subtree tree)))]
      [(equal? x current)(display "Item Found")]
       )
   )))

;;D
(define (insert_list list tree)
  (if (empty? list) tree
      (insert_list (cdr list) (insert (car list) tree)))
  )
;;E
(define (t_sort list)
    (traverse (insert_list list '()))
  )
;F
(define (insert_list_desc x tree)
  (if (null? tree)
   (node x '() '())
   (let ((current (root tree)))
     (cond
      [(> x current)(node current (insert_list_desc x (left_subtree tree)) (right_subtree tree) )]
      [(< x current)(node current (left_subtree tree) (insert_list_desc x (right_subtree tree)))]
      [(equal? x current)(display "Item Found")]
       )
   )))

(define (insert_list_ld x tree)
  (if (null? tree)
   (node x '() '())
   (let ((current (root tree)))
     (cond
      [(< (remainder x 10)(remainder current 10))(node current (insert_list_ld x (left_subtree tree)) (right_subtree tree) )]
      [(> (remainder x 10)(remainder current 10))(node current (left_subtree tree) (insert_list_ld x (right_subtree tree)))]
      [(equal? x current)(display "Item Found")]
       )
   )))

(define (insert_list_d list tree)
  (if (empty? list) tree
      (insert_list_d (cdr list) (insert_list_desc (car list) tree)))
  )

(define (insert_list_last_digit list tree)
  (if (empty? list) tree
      (insert_list_last_digit (cdr list) (insert_list_ld (car list) tree)))
  )

(define (descending list)
  (traverse (insert_list_d list '()))
  )

(define (ascending list)
  (traverse (insert_list list '()))
  )

(define (ascending_last_digit list)
  (traverse (insert_list_last_digit list '()))
  )

(define (higher_order_sort list function)
  (function list)
  )

(define test_tree (node 8 ( node 3 ( node 1 '() '() ) (node 6 (node 4 '() '()) (node 7 '() '())))  (node 10 '() (node 14 (node 13 '() '()) '()))))
(display "A: Display in sorted order the contents of a binary search tree\n")
(traverse test_tree)
(display "\n")
(display "B: Return #t or #f if a given item is present or absent in a tree or not.\n")
(display "(1)\n")
(search 1 test_tree)
(display "(3)\n")
(search 3 test_tree)
(display "(15)\n")
(search 15 test_tree)
(display "\n")
(display "C: Insert an item correctly into a list representing a binary search tree (5)\n")
(traverse (insert 5 test_tree))
(display "\n")
(display "D: Take a list of items and insert them into a binary search tree.(15 16 17 18)
\n")
(traverse (insert_list (list 15 16 17 18) test_tree))
(display "\n")
(display "E: Implement a tree-sort algorithm (67 38 82 45 34)\n")
(t_sort (list 67 38 82 45 34))
(display "\n")
(display "F: Implement a higher order version of the tree-sort function that takes a list and a
function that determines the sorted order\n")
(display "Ascending (67 38 82 45 34) :\n")
(display "\n")
(higher_order_sort (list 67 38 82 45 34) ascending )
(display "Descending (67 38 82 45 34) \n")
(display "\n")
(higher_order_sort (list 67 38 82 45 34) descending )
(display "Ascending (last digit) (67 38 82 45 34) \n")
(display "\n")
(higher_order_sort (list 67 38 82 45 34) ascending_last_digit)
