#lang scheme/base

(require scheme/mpair)
(require math/number-theory)
(require racket/hash)

; (read-line)

(define (parseStr str)
  (define (addSpaces str)
    (regexp-replace* #rx"([.!?-])" str " \\1 ")
  )
  (define (splitBySpaces str)
    (regexp-split #rx" +" str)
  )
  (define (removeEmptyStr strLst)
    (filter 
      (lambda (str) (not (equal? str "")))
      strLst
    )
  )
  (define (mapStringToSymbol strArr) (map string->symbol strArr))

  (mapStringToSymbol (removeEmptyStr (splitBySpaces (addSpaces str))))
)


(parseStr "Hello world! I am there! And I am the king!!!  ")