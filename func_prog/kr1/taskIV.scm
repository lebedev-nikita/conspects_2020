#lang scheme/base

(define empty-tree #())
(define make-tree vector)
(define tree? vector?)

(define (tree-topleft tree) (vector-ref tree 0))
(define (tree-topright tree) (vector-ref tree 1))
(define (tree-botleft tree) (vector-ref tree 2))
(define (tree-botright tree) (vector-ref tree 3))

(define (taskIV t s cc)
  (if (not (tree? t))
    (cc (* s t))
    (cc 
      (+
        (taskIV (tree-topleft t) (/ s 4))
        (taskIV (tree-topright t) (/ s 4))
        (taskIV (tree-botleft t) (/ s 4))
        (taskIV (tree-botright t) (/ s 4))
      )
    )
  )
)

(lambda (w) 
  (cc 
    (+
      w
      (taskIV (tree-topright t) (/ s 4))
      (taskIV (tree-botleft t) (/ s 4))
      (taskIV (tree-botright t) (/ s 4))
    )
  )
)

(lambda (x) 
  (cc 
    (+ w x
      (taskIV (tree-botleft t) (/ s 4))
      (taskIV (tree-botright t) (/ s 4))
    )
  )
)

(lambda (y) 
  (cc 
    (+ w x y
      (taskIV (tree-botright t) (/ s 4))
    )
  )
)
(lambda (z) 
  (cc 
    (+ w x y z)
  )
)

(taskIV 
  #(1 0 0 #(1 1 1 0)) 
  16
  (lambda (x s) (* s x))
)


; (lambda (w) 
;   (cc (+ w
;     (taskIV (tree-topright t) (/ s 4))
;     (taskIV (tree-botleft t) (/ s 4))
;     (taskIV (tree-botright t) (/ s 4))
;   ))
; )
; (lambda (x) 
;   (cc (+ w x
;     (taskIV (tree-botleft t) (/ s 4))
;     (taskIV (tree-botright t) (/ s 4))
;   ))
; )
; (lambda (y) 
;   (cc (+ w x y
;     (taskIV (tree-botright t) (/ s 4))
;   ))
; )
; (lambda (z) 
;   (cc (+ w x y z)) 
; )

; (define (taskIV t s cc)
;   (if (not (tree? t))
;     (* s t)
;     (+
;       (taskIV (tree-topleft t) (/ s 4))
;       (taskIV (tree-topright t) (/ s 4))
;       (taskIV (tree-botleft t) (/ s 4))
;       (taskIV (tree-botright t) (/ s 4))
;     )
;   )
; )
