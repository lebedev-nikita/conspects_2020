#lang scheme/base
; b
; (define (2n-1!-list n) 
;   (if (or (<= n 0) (not (integer? n)))
;     '()
;     (let fact )


;   )
; )
(define (fact2 n lst) 
  (if (= n 0)
    lst
    (if (= 0 (modulo n 2))
        (fact2 (- n 1) lst)
        (fact2 (- n 1) (cons (fact n 1) lst)
      )
    )
  )
)

(define (fact n mul) 
  (if (= n 0)
    mul
    (fact (- n 1) (* mul n))
  )
)
