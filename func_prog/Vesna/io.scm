#lang scheme/base

(require racket/string) ;+
(require racket/port) ;+

(require scheme/mpair) 
(require racket/hash) ;+

; (read-line)
;[.,:;()!?-] символы пунктуации

(define (parseString str)
  (define (removeBadChars str)
    (regexp-replace* #rx"([&\\^\\*\\+\\=\\_\\|\\/\\%\\$\\#\\№\\@\\>\\<\\`\\~\\{\\}]|[[]|[]]|\n)" 
      (regexp-replace* #rx"(\n)" str ". ")
      " "
    )
  )
  (define (addSpaces str)
    (regexp-replace* #rx"([.,:;«»()!?-])" str " \\1 ")
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

  (mapStringToSymbol (removeEmptyStr (splitBySpaces (addSpaces (removeBadChars (string-downcase str))))))
)

(define (prepareForPrint symbolList)
  (define (mapSymbolToString symbolList) (map symbol->string symbolList))
  (define (removeOddSpaces str)
    (regexp-replace* #rx" +([.,:;»)!?-])" str "\\1") ; убрал открывающую скобку
  )
  
  (removeOddSpaces(string-join (mapSymbolToString symbolList)))
)

(define (readFile path)
  (let* 
    (
      (inputPort (open-input-file path))
      (data (port->string inputPort))
    )
    (close-input-port inputPort)
    data
  )
)

; (hash-ref <hash> <key> <failval>)
; (hash-set! <hash> <key> <val>) 
; val: (prevs . nexts)

(define mainHashTable (make-hash))

(define (addSymbolToMainHashTable symbol)
  (let 
    (
      (prevs.nexts (hash-ref mainHashTable symbol #f))
    )
    (if prevs.nexts
      prevs.nexts
      (let ((prevs.nexts (cons (make-hash) (make-hash))))
        (hash-set! mainHashTable symbol prevs.nexts)
        prevs.nexts
      )
    )
  )
)

(define (addPrev symbol prev)
  (let* 
    (
      (prevs (car (addSymbolToMainHashTable symbol)))
      (count (hash-ref prevs prev 0))
    )
    (hash-set! prevs prev (+ 1 count))
  )
)

(define (addNext symbol next)
  (let* 
    (
      (nexts (cdr (addSymbolToMainHashTable symbol)))
      (count (hash-ref nexts next 0))
    )
    (hash-set! nexts next (+ 1 count))
  )
)

(define (learn symbolList prev)
  (if (null? symbolList)
    "finished"
    ; mainHashTable
    (let ((cur (car symbolList)))
      (addNext prev cur)
      (addPrev cur prev)
      (learn (cdr symbolList) cur)
    )
  )
)


; (parseString (readFile "./freud.txt"))
(learn (parseString (readFile "./freud.txt")) '|.|)

; (cdr (hash-ref mainHashTable 'found 0))