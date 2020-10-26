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

(define (readFileAsString path)
  (let* 
    (
      (inputPort (open-input-file path))
      (data (port->string inputPort))
    )
    (close-input-port inputPort)
    data
  )
)

(define (readFileAsObject path)
  (let* 
    (
      (inputPort (open-input-file path))
      (data (read inputPort))
    )
    (close-input-port inputPort)
    data
  )
)

; (hash-ref <hash> <key> <failval>)
; (hash-set! <hash> <key> <val>) 
; val: (prevs . nexts)

(define (addKeyToHashTable hashTable symbol)
  (let 
    (
      (prevs.nexts (hash-ref hashTable symbol #f))
    )
    (if prevs.nexts
      prevs.nexts
      (let ((prevs.nexts (cons (make-hash) (make-hash))))
        (hash-set! hashTable symbol prevs.nexts)
        prevs.nexts
      )
    )
  )
)

(define (addPrev hashTable symbol prev increment)
  (let* 
    (
      (prevs (car (addKeyToHashTable hashTable symbol)))
      (count (hash-ref prevs prev 0))
    )
    (hash-set! prevs prev (+ increment count))
  )
)

(define (addNext hashTable symbol next increment)
  (let* 
    (
      (nexts (cdr (addKeyToHashTable hashTable symbol)))
      (count (hash-ref nexts next 0))
    )
    (hash-set! nexts next (+ increment count))
  )
)

(define (learn hashTable symbolList prev)
  (if (null? symbolList)
    "learn finished"
    (let ((cur (car symbolList)))
      (addNext hashTable prev cur 1)
      (addPrev hashTable cur prev 1)
      (learn hashTable (cdr symbolList) cur)
    )
  )
)

(define (reWriteFile data path)
  (let* 
    (
      (outputPort (open-output-file path #:exists 'replace))
    )
    (print data outputPort)
    (close-output-port outputPort)
    "writeFile finished"
  )
)

(define (mergeHashTables ht1 ht2)
  (hash-for-each ht2
    (lambda (symbol prevs.nexts)
      (hash-for-each (car prevs.nexts)
        (lambda (prev count) 
          (addPrev ht1 symbol prev count)
        )
      )
      (hash-for-each (cdr prevs.nexts)
        (lambda (next count) 
          (addNext ht1 symbol next count)
        )
      )
    )
  )
  "mergeHashTables finished"
)


(define myHashTable (make-hash))

; (parseString (readFileAsString "./freud.txt"))
(learn myHashTable (parseString (readFileAsString "freud.txt")) '|.|)
(reWriteFile myHashTable "output.txt")
(define n (readFileAsObject "output.txt"))

; (mergeHashTables n myHashTable)
(mergeHashTables myHashTable n) 
; NOTE: не работает с обратным порядком параметров. Скорее всего, при записи и чтении 
; теряется мутабельность хэша.

(hash-ref (cdr (hash-ref n 'found)) 'in)
(hash-ref (cdr (hash-ref myHashTable 'found)) 'in)

; (cdr (hash-ref myHashTable 'found 0)) ; выводим nexts

