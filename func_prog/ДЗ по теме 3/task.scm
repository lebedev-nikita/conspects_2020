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


(define (sum lst)
  (foldl + 0 lst)
)

(define (mul lst)
  (foldl * 1 lst)
)

(define (process lst)
  (let ((first-mul (mul (car lst))))
    (filter
      (lambda (sublst)
        (< first-mul (sum sublst))
      )
      lst
    )
  )
)

; (fib-list 1)
; (fib-list 2)
; (fib-list 3)
; (fib-list 4)
; (fib-list 5)
; (fib-list 6)
; (fib-list 7)
; (fib-list 8)
; (list-fib-squares-a 8)
; (list-fib-squares-b 8)

; (sum '(1 2 3))
; (mul '(1 2 3))

(process '((1 2 3) (2 3 5) (1 1 1)))
