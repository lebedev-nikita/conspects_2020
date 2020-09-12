#lang scheme/base

; a
(define (iter n)
  (if (or (<= n 0) (not (integer? n)))
    '()
    (let reverse 
      (
        (prev '())
        (next 
          (let forward
            (
              (this 1)
              (maxThis (- (* 2 n) 1))
              (lst '())
              (mul 1)
            )
            (if (= maxThis this)
              (cons (* this mul) lst)
              (if (= 0 (modulo this 2))
                (forward (+ 1 this) maxThis lst (* mul this))
                (forward (+ 1 this) maxThis (cons (* this mul) lst) (* mul this))
              )
            )
          )
        )
      )
      (if (null? next)
        prev
        (reverse (cons (car next) prev) (cdr next))
      )
    )
  )
)

; b
(define (lin n)
  (define (fact-lst nThis nMax mul)
    (if (= nThis nMax)
      (cons (* mul nThis) '())
      (if (= 0 (modulo nThis 2))
        (fact-lst (+ 1 nThis) nMax (* mul nThis))
        (cons (* mul nThis) (fact-lst (+ 1 nThis) nMax (* mul nThis)))
      )
    )
  )
  (if (or (<= n 0) (not (integer? n)))
    '()
    (fact-lst 1 (- (* 2 n) 1) 1)
  )
)
