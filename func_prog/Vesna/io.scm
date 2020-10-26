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

(define (getNextCount hashTable symbol next) ; ломается при отсутствующем symbol
  (let* 
    (
      (nexts (cdr (hash-ref hashTable symbol 'noghing))) 
      (count (hash-ref nexts next 0))
    )
    count
  )
)

(define (getPrevCount hashTable symbol prev) ; ломается при отсутствующем symbol
  (let* 
    (
      (prevs (car (hash-ref hashTable symbol 'noghing))) 
      (count (hash-ref prevs prev 0))
    )
    count
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
  (define (mergeKey key)
    (let*
      (
        (ht2pKeys (hash-keys (car (hash-ref ht2 key 'nothing))))
        (ht2nKeys (hash-keys (cdr (hash-ref ht2 key 'nothing))))
      )
      (for-each
        (lambda (ht2pKey) 
          (addPrev ht1 key ht2pKey (getPrevCount ht2 key ht2pKey))
        )
        ht2pKeys
      )
      (for-each
        (lambda (ht2nKey) 
          (addNext ht1 key ht2nKey (getNextCount ht2 key ht2nKey))
        )
        ht2nKeys
      )
    )
  )
  (let 
    (
      (ht2Keys (hash-keys ht2))
    )
    (for-each
      mergeKey
      ht2Keys
    )
    "mergeHashTables finished"
  )
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

(getNextCount n 'found 'in)
(getNextCount myHashTable 'found 'in)

; (cdr (hash-ref myHashTable 'found 0)) ; выводим nexts

