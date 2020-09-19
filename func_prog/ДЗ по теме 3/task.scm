#lang scheme/base

(define (fib-list n)
  (define (fun lst i n)
    (if (> i n)
      lst
      (let ((next (+ (car lst) (cadr lst))))
        (fun
          (cons next lst)
          (+ i 1)
          n
        )
      )
    )
  )
  (reverse 
    (if (= n 1)
      '(0)
      (if (= n 2)
        '(1 0)
        (fun '(1 0) 3 n)
      )
    )
  )
)

(define (list-fib-squares-a n)
  (map
    (lambda (x) (* x x))
    (fib-list n)
  )
)

(define (list-fib-squares-b n)
  (reverse 
    (foldl
      (lambda (new init) 
        (cons (* new new) init)
      )
      '()
      (fib-list n)
    )
  )
)

(define (process lst)

)

(fib-list 1)
(fib-list 2)
(fib-list 3)
(fib-list 4)
(fib-list 5)
(fib-list 6)
(fib-list 7)
(fib-list 8)
(list-fib-squares-a 8)
(list-fib-squares-b 8)
