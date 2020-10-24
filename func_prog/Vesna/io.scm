#lang scheme/base

(require scheme/mpair)
(require math/number-theory)
(require racket/hash)
(require racket/string) ;+

; (read-line)
;[.,:;()!?-] символы пунктуации

(define (string->symbolList str)
  (define (removeBadChars str)
    (regexp-replace* #rx"([&\\^\\*\\+\\=\\_\\|\\/\\%\\$\\#\\№\\@\\>\\<\\`\\~\\{\\}]|[[]|[]])" str "")
  )
  (define (addSpaces str)
    (regexp-replace* #rx"([.,:;()!?-])" str " \\1 ")
  )
  (define (splitBySpaces str)
    (regexp-split #rx" +" str)
  )
  (define (removeEmptyStr strList)
    (filter 
      non-empty-string?
      strList
    )
  )
  (define (mapStringToSymbol strList) (map string->symbol strList))

  (mapStringToSymbol (removeEmptyStr (splitBySpaces (addSpaces (removeBadChars str)))))
)

(define (symbolList->string symbolList)
  (define (mapSymbolToString symbolList) (map symbol->string symbolList))
  (define (removeOddSpaces str)
    (regexp-replace* #rx" +([.,:;)!?-])" str "\\1") ; убрал открывающую скобку
  )
  
  (removeOddSpaces(string-join (mapSymbolToString symbolList)))
)

(define str1 "fdk()ldksl")
(define str2 "Hello world! I am there! And [&&] I am the king!!!  ")

(string->symbolList str1)
(symbolList->string (string->symbolList str1))
(string->symbolList str2)
(symbolList->string (string->symbolList str2))

; (string->symbolList (read-line))
; (println (list 'a 'b 'c 'd))