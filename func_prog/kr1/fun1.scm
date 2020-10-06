#lang scheme/base

(define (fun1 lst)
  (if (null? lst) 
    '()
    (let (
      (lstMax 
        (foldl 
          (lambda (cur prev) 
            (if (> cur prev)
              cur
              prev
            )
          )
          (car lst)
          (cdr lst)
        )
      ))  
      (foldl 
        (lambda (cur prev) 
          (if (and (< cur lstMax) (or (equal? prev '()) (> cur prev)))
            cur
            prev
          )
        )
        '()
        lst
      )
    )
  )
)

(fun1 (list))
(fun1 '(1 1))
(fun1 (list -1 0 1 -1 0 1 -1))
