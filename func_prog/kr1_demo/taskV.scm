#lang scheme/base

(define (taskV . args)
  (define (superpos funcList cc)
    (let 
      (
        (thisFunc (car funcList))
        (otherFuncs (cdr funcList))
      )
      (if (null? otherFuncs)
        (cc thisFunc)
        ; (cc (lambda (x) ((superpos otherFuncs) (thisFunc x))))
        ; (lambda (y) (cc (lambda (x) (y (thisFunc x)))))
        (superpos otherFuncs (lambda (y) (cc (lambda (x) (y (thisFunc x))))))
      )
    )
  )
  (superpos args (lambda (x) x))
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
