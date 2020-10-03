#lang scheme/base

(define (taskIII lst) (let*
  (
    (longestLists (cdr (foldl
      (lambda (cur prev)
        (let 
          (
            (prevResults (cdr prev))
            (prevMaxLength (caar prev))
            (curNum (+ 1 (cadar prev)))
            (curLength (length cur))
          )
          (cond 
            ((> curLength prevMaxLength)
              (cons (list curLength curNum) (list curNum))
            )
            ((= curLength prevMaxLength)
              (cons (list prevMaxLength curNum) (cons curNum prevResults))
            )
            ((< curLength prevMaxLength)
              (cons (list prevMaxLength curNum) prevResults)
            )
          )
        )
      )
      ;((maxLength curNum) ...nums)
      (list 
        (list 
          (length (car lst))
          0
        )
        0
      )
      (cdr lst)
    )))
    ;
    (revResult (cdr (foldl
      (lambda (cur prev)
        (let*
          (
            (curIndex (car prev))
            (formedList (cdr prev))
            (newThis (if (not (member curIndex longestLists))
              cur
              (map add1 cur)
            ))
          )
          (cons (+ 1 curIndex) (cons newThis formedList))
        )
      )
      ; (curIndex ...formedList)
      '(0)
      lst
    )))
  )
  (reverse revResult)
))
