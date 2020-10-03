#lang scheme/base

(define empty-tree #())
(define make-tree vector)
(define tree? vector?)

(define (tree-topleft tree) (vector-ref tree 0))
(define (tree-topright tree) (vector-ref tree 1))
(define (tree-botleft tree) (vector-ref tree 2))
(define (tree-botright tree) (vector-ref tree 3))

(define (taskII t s)
  (if (not (tree? t))
    (* s t)
    (let ((s4 (/ s 4)))
      (+
        (taskII (tree-topleft t) s4)
        (taskII (tree-topright t) s4)
        (taskII (tree-botleft t) s4)
        (taskII (tree-botright t) s4)
      )
    )
  )
)
