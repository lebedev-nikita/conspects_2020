#lang scheme/base

(define (vect-len lst)
  (let* 
    (
      (square-lst (map 
        (lambda (x) (* x x))
        lst
      ))
      (square (foldl + 0 square-lst))
    )
    (sqrt square)
  )
)

(define (task-03-2020 lst)
  (let* 
    (
      (len-lst (map vect-len lst))
      (mul (foldl * 1 len-lst))
    )
    (expt mul (/ 1 (length lst)))
  )
)

(task-03-2020 '((3 4) (3 4) (3 4)) )
