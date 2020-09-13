#lang scheme/base

(define (opposite x1 y1 z1 x2 y2 z2)
  (let*
    (
      (i (- (* y1 z2) (* y2 z1)))
      (j (- (* x1 z2) (* x2 z1)))
      (k (- (* x1 y2) (* x2 y1)))
      (are-collinear (= 0 i j k))
    )
    (and 
      are-collinear 
      (or 
        (< (* x1 x2) 0) 
        (< (* y1 y2) 0) 
        (< (* z1 z2) 0)
        (= 0 x1 y1 z1 x2 y2 z2)
      )
    )
  )
)

; #t
(opposite 0 0 0 0 0 0)
(opposite 1 1 1 -1 -1 -1)
(opposite 1 0 0 -2 0 0)
(opposite 1 1 1/2 -2 -2 -1)
; #f
(opposite 1 1 0 1 1 0)
(opposite 1 0 0 -2 -2 0)
(opposite 1 1 -1/2 -2 -2 -1)
