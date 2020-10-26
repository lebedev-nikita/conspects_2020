#lang scheme/base

(require racket/string) ;+
(require racket/port) ;+

(require scheme/mpair) 
(require racket/hash) ;+
(require racket/list) ;+

; (read-line)
;[.,:;()!?-] символы пунктуации

(define (parseString str)
  (define (removeBadChars str)
    (regexp-replace* #rx"([&\\^\\*\\+\\=\\_\\|\\/\\%\\$\\#\\№\\@\\>\\<\\`\\~\\{\\}\"]|[[]|[]]|\n)" 
      (regexp-replace* #rx"(\n)" str " ")
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

(define (isEnd symbol)
  (or
    (equal? symbol '|.|)
    (equal? symbol '|?|)
    (equal? symbol '|!|)
  )
)

(define (incCount hashTable key increment)
  (let*
    (
      (count (hash-ref hashTable key 0))
      (newCount (+ increment count))
    )
    (hash-set! hashTable key newCount)
    newCount
  )
)


(define (createNgramGenereator N symbolList)
  (define n-1 (- N 1))

  (define (pop)
    (if (null? symbolList)
      #f
      (let ((elem (car symbolList)))
        (set! symbolList (cdr symbolList))
        elem
      )
    )
  )

  (define prev '|.|)
  (define cur '())
  (define last "empty")
  (define next (pop))

  (define (shift)
    (set! prev (car cur))
    (set! cur (append (cdr cur) (list next)))
    (set! last next)
    (set! next (pop))
  )

  (define (fill)
    (set! cur (append cur (list next)))
    (set! last next)
    (set! next (pop))
  )

  (define (firstFill)
    (if (= n-1 (length cur))
      "firstFill finished";
      (begin
        (fill)
        (firstFill)
      )
    )
  )
  (firstFill)

  (define (endInside lst) 
    (ormap isEnd lst)
  )

  (define (getNgram)
    (let ((res (list prev cur next)))
      (if (not next)
        res
        (if (endInside cur)
          (begin 
            (shift)
            (getNgram)
          )
          (begin
            (shift)
            res
          )
        )
      )
    )
  )
  getNgram
  ; (prev (cur: n-1) next)
)

(define (learn firsts followings N symbolList)
  (define generate (createNgramGenereator N symbolList))
  (define (recFun)
    (let*
      (
        (prevCurNext (generate))
        (prev (car prevCurNext))
        (cur (cadr prevCurNext))
        (next (caddr prevCurNext))
      )
      (if (not next)
        "learn finished"
        (begin
          (if (isEnd prev)
            (incCount firsts cur 1)
            "nothing"
          )
          (addNext followings cur next 1)
          (addPrev followings cur prev 1)
          (recFun)
        )
      )
    )
  )
  (recFun)
)

(define (pickRandomKeyFromHT hashTable)
  (define sum 0)
  (hash-for-each hashTable (lambda (key count) (set! sum (+ count sum))))
  (define rand (random sum))
  (define result "empty")
  (hash-for-each hashTable 
    (lambda (key count) 
      (if (not (equal? result "empty"))
        "nothing"
        (begin 
          (set! rand (- rand count))
          (if (<= rand 0)
            (set! result key)
            "nothing"
          )
        )
      )
    )
  )
  result
)

(define (mergeHashTables ht1 ht2) ; TODO
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


(define firsts (make-hash))
(define followings (make-hash))
(learn firsts followings 2 (parseString (readFileAsString "freud.txt")))

; (hash-ref followings '(found) 0)
firsts

; (parseString (readFileAsString "./freud.txt"))
; (reWriteFile myHashTable "output.txt")
; (define n (readFileAsObject "output.txt"))

; (mergeHashTables n myHashTable)
; (mergeHashTables myHashTable n) 
; NOTE: не работает с обратным порядком параметров. Скорее всего, при записи и чтении 
; теряется мутабельность хэша.
