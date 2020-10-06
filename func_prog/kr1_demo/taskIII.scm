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


При нормальном порядке параметры функции не вычисляются, пока не они не понадобятся.
При аппликативном порядке сначала вычсляются параметры, затем к ним применяется функция.
При аппликативном порядке вычислятся даже те аргументы функции, 
которые внутри нее не используются. 
Нормальный порядок позволяет: 
1. Не вычислять одинаковые параметры повторно
2. Избежать тех ошибок, которые могут произойти при вычислении неиспользуемых параметров

(define (f a b) (+ a b))
(f (f 1 2) (f 3 4))

Нормальный:
(f (f 1 2) (f 3 4))  |
(+ (f 1 2) (f 3 4))  | подстановка
(+ (+ 1 2) (+ 3 4))  |
(+ 3 7)     | редукция
10          |

Аппликативный:
(f (f 1 2) (f 3 4))
(f (+ 1 2) (+ 3 4))
(f 3 7)
(+ 3 7)
10
