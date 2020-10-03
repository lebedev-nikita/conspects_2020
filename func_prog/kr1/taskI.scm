#lang scheme/base
(define (taskI lst)
  (if (null? lst)
    '()
    (cdr 
      (foldl 
        (lambda (cur prev) 
          (let 
            (
              (prevRes (cdr prev))
              (prevMin (caar prev))
              (curNum (+ 1 (cadar prev)))
            )
            (cond 
              ((= cur prevMin) 
                (cons (list prevMin curNum) (cons curNum prevRes))
              )
              ((> cur prevMin) 
                (cons (list prevMin curNum) prevRes)
              )
              ((< cur prevMin) 
                (cons (list cur curNum) (list curNum))
              )
            )
          )
        )
        ; ((min prevNum) (resList))
        (list (list (car lst) 0) 0)
        (cdr lst)
      )
    )
  )
)

; (taskl '(-1 0 1 -1 0 1 -1))


; (define (test lst)
;   (list 
;     (foldl
;       (lambda (cur prev) (cons cur prev))
;       '()
;       lst
;     )
;     (foldr
;       (lambda (cur prev) (cons cur prev))
;       '()
;       lst
;     )
;   )
; )
