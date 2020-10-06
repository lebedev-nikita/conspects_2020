#lang scheme/base

(define (simple? n)
  (define (simpleInner? n i stopI)
    (if (> i stopI)
      #t
      (if (= 0 (modulo n i))
        #f
        (simpleInner? n (+ 1 i) stopI)
      )
    )
  )
  (simpleInner? n 2 (sqrt n))
)

(define (fun3 n)
  (print n)
  (print " ")
  (define (fun3Inner i mul curNum maxNum)
    ; (if (>= curNum maxNum)
    ; )
    (if (simple? i)
      (fun3Inner (+ 1 i) mul curNum maxNum)
      (if (< curNum maxNum)
        (fun3Inner (+ 1 i) (* i mul) (+ 1 curNum) maxNum)
        (* i mul)
      )
    )
  )
  (fun3Inner 1 1 1 n)
)

(fun3 0)
(fun3 1)
(fun3 2)
(fun3 3)
(fun3 4)
(fun3 5)
(fun3 6)
(fun3 7)
(fun3 8)
(fun3 9)
(fun3 10)
; (fun3 0) => 1    
; (fun3 4) => 1728    
; (fun3 10) => 12541132800
