#lang scheme/base

(define empty-tree #())
(define make-tree vector)
(define (tree-data tree) (vector-ref tree 0))
(define (tree-left tree) (vector-ref tree 1))
(define (tree-right tree) (vector-ref tree 2))
(define (tree-empty? t) (equal? t #()))

(define (task-4-2020 h)
  (if (= 0 h)
    empty-tree
    (let* 
      (
        (tree1 (task-4-2020 (- h 1)))
        (tree2 (task-4-2020 (- h 1)))
        (value (if (tree-empty? tree1) 1 (* 2 (tree-data tree1))))
      )
      (make-tree value tree1 tree2)
    )
  )
)


(task-4-2020 0)
(task-4-2020 1)
(task-4-2020 2)
(task-4-2020 3)
