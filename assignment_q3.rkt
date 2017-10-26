#lang racket
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

(define test_tree (node 8 ( node 3 ( node 1 '() '() ) (node 6 (node 4 '() '()) (node 7 '() '())))  (node 10 '() (node 14 (node 13 '() '()) '()))))

(define empty_tree (node '() '() '()))

(define (traverse tree)
  (if(null? tree)
  '()
  (begin
    (traverse (left_subtree tree))
    (display (root tree))(newline)
    (traverse (right_subtree tree))
  )))


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

(traverse test_tree) ;;workss
(search 1 test_tree)
(search 3 test_tree)
(search 4 test_tree)
(search 6 test_tree)
(search 7 test_tree)
(search 8 test_tree)
(search 10 test_tree)
(search 13 test_tree)
(search 14 test_tree)
(search 15 test_tree);;should be false

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


(traverse (insert 5 test_tree))




  

