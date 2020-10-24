#lang scheme/base

(require scheme/mpair)
(require math/number-theory)
(require racket/hash)

(define (isDeficient? n)
  (< (divisor-sum n 1) (* 2 n))
)

(define (evenDeficient n)
  (define (inner n prevNum)
    (if (= 0 n)
      prevNum
      (let ((curNum (+ 2 prevNum)))
        (if (isDeficient? curNum)
          (inner (- n 1) curNum)
          (inner n curNum)
        )
      )
    )
  )
  (inner n 0)
)

; (hash-ref <hash> <key> <failval>)
; (hash-set! <hash> <key> <val>) 

(define h (make-hash))
(define maxHashIndex 0)

(define (memoEvenDeficient n)
  (define (inner n prevIndex prevNum)
    (if (= prevIndex n)
      prevNum
      (let ((curNum (+ 2 prevNum)))
        (if (isDeficient? curNum)
          (begin
            (hash-set! h (+ 1 prevIndex) curNum)
            (set! maxHashIndex (+ 1 prevIndex))
            ; (println (list "Занесено в хэш: " curNum "Под индексом: " maxHashIndex))
            (inner n (+ 1 prevIndex) curNum)
          )
          (inner n prevIndex curNum)
        )
      )
    )
  )

  (if (<= n maxHashIndex)
    (begin
      ; (println (list "Взято из хэша по индексу: " n))
      (hash-ref h n 'nothing)
    )
    (inner n maxHashIndex (hash-ref h maxHashIndex 0))
  )
)

Краткое описание работы моей мемоизированной функции с хэш-таблицей:
Значение maxHashIndex - максимальный номер элемента последовательности, 
значение которого занесено в хэш-таблицу.
Вычисляя n-ный элемент последовательности, функция вычисляет заносит в таблицу
все предшествующие ему элементы, не занесенные в таблицу ранее.
Таким образом, все элементы с индексами от 1 до maxHashIndex 
оказываются занесены в таблицу.

Ответ:
Мемоизированная версия дает выигрыш по сравнению с обычной при некоторых условиях.
Реализованная мною мемоизированная функция позволяет за константное время получить
результат для натурального числа n, если это число не превышает наибольшее (maxHashIndex) 
из обработанных ранее чисел.
В случае, когда число n всё-таки превышает maxHashIndex, требуется вычислить 
только (n - maxHashIndex) последних предшествующих n-ному членов последовательности.
При этом, занесение в хэш-таблицу новых элементов отнимает некоторое количество 
времени и незначительное количество памяти, поэтому при 
одноразовом вычислении результата для n, значительно превышающего maxHashIndex, 
функция будет работать менее эффективно.



; (evenDeficient 1)
; (evenDeficient 2)
; (evenDeficient 3)
; (evenDeficient 4)
; (evenDeficient 5)

; (memoEvenDeficient 5)
; (memoEvenDeficient 1)
; (memoEvenDeficient 2)
; (memoEvenDeficient 3)
; (memoEvenDeficient 4)
; (memoEvenDeficient 5)
; (memoEvenDeficient 1)





