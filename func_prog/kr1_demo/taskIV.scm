#lang scheme/base

(define empty-tree #())
(define make-tree vector)
(define tree? vector?)

(define (tree1 tree) (vector-ref tree 0))
(define (tree2 tree) (vector-ref tree 1))
(define (tree3 tree) (vector-ref tree 2))
(define (tree4 tree) (vector-ref tree 3))

(define (taskIV t s cc)
  (if (not (tree? t))
    (cc (* s t))
    (taskIV 
      (tree1 t) 
      (/ s 4)
      (lambda (w) 
        (taskIV 
          (tree2 t) 
          (/ s 4)
          (lambda (x) 
            (taskIV 
              (tree3 t) 
              (/ s 4)
              (lambda (y) 
                (taskIV 
                  (tree4 t) 
                    (/ s 4)
                    (lambda (z) 
                      (cc (+ w x y z)))))))))
    )
  )
)

(taskIV 
  #(1 0 0 #(1 1 1 0)) 
  16 
  (lambda(x) x) 
)
