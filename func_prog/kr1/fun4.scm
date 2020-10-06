#lang scheme/base

(define emptyTree #())
(define makeTree vector)
(define tree? vector?)
(define (treeEmpty? tree) (equal? tree #()))

(define (treeData tree) (vector-ref tree 0))
(define (treeLeft tree) (vector-ref tree 1))
(define (treeRight tree) (vector-ref tree 2))

(define (fun4 tree r1 r2)
  (define (fun4Inner tree r1 r2 rootDistance)
    (if (treeEmpty? tree)
      0
      (+ 
        (fun4Inner (treeLeft tree) r1 r2 (+ 1 rootDistance)) 
        (fun4Inner (treeRight tree) r1 r2 (+ 1 rootDistance))
        (if (and (between? rootDistance r1 r2) (> (treeData tree) 0))
          1
          0
        )
      )
    )
  )
  (fun4Inner tree r1 r2 0)
)

(define (between? x r1 r2)
  (let 
    (
      (min (if (< r1 r2) r1 r2))
      (max (if (< r1 r2) r2 r1))
    )
    (and (<= x max) (>= x min))
  )
)

(fun4 #(1 #(-1 #(3 #() #()) #(3 #() #())) #(-2 #() #())) 2 1)
; => 2
(fun4 #() 1 10)
; => 0    
(fun4 #(10 #() #()) 0 0)
; => 1
