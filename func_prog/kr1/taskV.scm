#lang scheme/base

(define (taskV . args)
  (define (superpos funcList)
    (let 
      (
        (thisFunc (car funcList))
        (otherFuncs (cdr funcList))
      )
      (if (null? otherFuncs)
        thisFunc
        (lambda (x) ((superpos otherFuncs) (thisFunc x)))
      )
    )
  )
  (superpos args)
)

(define f 
  (taskV 
    (lambda (x) (+ 1 x))
    (lambda (x) (* 2 x))
    (lambda (x) (- x 3))
  )
)

(f 1)
(f 2)
(f 3)
(f 4)
(f 5)
