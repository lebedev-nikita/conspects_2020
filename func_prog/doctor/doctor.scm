#lang scheme/base

(require racket/string)
(require racket/port)
(require racket/hash)
(require racket/list)

; параметр num - сколько еще пациентов доктор может принять
(define (visit-doctor stopWord num starts.order)
  (define (askPatientName)
    (begin
      (printf "next!\n")
      (printf "who are you?\n")
      (printf "** ")
      (car (parseString (read-line)))
    ) 
  )
  (if (>= 0 num)
    '(time to go home)
    (let ((name (askPatientName)))
      (if (equal? name stopWord)
        (printf "working day finished")
        (begin
          (printf "hello, ~a!\n" name)
          (printf "what seems to be the trouble?")
          (doctorDriverLoop name '() starts.order)
          (visit-doctor stopWord (- num 1) starts.order)
        )
      )
    )
  )
)

(define (doctorDriverLoop name oldPhrases starts.order)
  (newline)
  (printf "** ") ; доктор ждёт ввода реплики пациента, приглашением к которому является **
  (let ((userResponse (getFirstSentence (parseString (read-line)))))
    (cond 
      ((equal? userResponse '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
            (printf "Goodbye, ~a!\n" name)
            (print '(see you next week))
            (newline)
      )
      (else 
        (printf 
          (prepareForPrint
            (reply strategies (list
              (list 'userResponse userResponse) 
              (list 'oldPhrases oldPhrases)
              (list '|starts.order| starts.order)
            ))
          )
        ) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
        (doctorDriverLoop name (cons userResponse oldPhrases) starts.order)
      )
    )
  )
)

(define strategies (list
  ; (predicate weight function)
  (list 
    (lambda (assocParamList) #t) 
    1 
    (lambda (assocParamList)
      (let ((userResponse (cadr (assoc 'userResponse assocParamList))))
        (qualifierAnswer userResponse)
      )
    )
  )
  (list
    (lambda (assocParamList) #t) 
    2 
    (lambda (assocParamList) (hedge))
  )
  (list
    (lambda (assocParamList) (not (null? (cadr (assoc 'oldPhrases assocParamList))))) 
    3
    (lambda (assocParamList)
      (let ((oldPhrases (cadr (assoc 'oldPhrases assocParamList))))
        (historyAnswer oldPhrases)
      )
    )
  )
  (list
    (lambda (assocParamList) 
      (let ((userResponse (cadr (assoc 'userResponse assocParamList))))
        (hasKeywords userResponse)
      )
    ) 
    4
    (lambda (assocParamList)
      (let ((userResponse (cadr (assoc 'userResponse assocParamList))))
        (keywordAnswer userResponse)
      )
    )
  )
  (list 
    (lambda (assocParamList) #t) 
    1000 
    (lambda (assocParamList)
      (let* 
        (
          (starts.order (cadr (assoc '|starts.order| assocParamList)))
          (userResponse (cadr (assoc 'userResponse assocParamList)))
        )
        (generateAnswer starts.order userResponse)
      )
    )
  )
))

; генерация ответной реплики по userResponse - реплике от пользователя 
(define (reply strategies assocParamsLst)
  (define (filterByPredicate strategies assocParamList)
    (filter  
      (lambda (strtg) 
        (let ((predicate (car strtg)))
          (predicate assocParamList)
        )
      )
      strategies
    )
  )
  ; lst: ((weight function) (weight function) ...)
  (define (findByWeightAndRand lst rand)
    (let* 
      (
        (this (car lst))
        (thisWeight (car this))
        (thisFunction (cadr this))
      )
      (if (< rand thisWeight)
        thisFunction
        (findByWeightAndRand (cdr lst) (- rand thisWeight))
      )
    )
  )
  ; lst: ((weight function) (weight function) ...)
  (define (pickRandomWithWeight lst)
    (let* 
      (
        (maxRandom
          (foldl 
            (lambda (x init) (+ init (car x)))
            0
            lst
          )
        )
        (rand (random maxRandom))
      )
      (findByWeightAndRand lst rand)
    )
  )

  (let* 
    (
      (filteredStrategies (filterByPredicate strategies assocParamsLst))
      (weightedList (map cdr filteredStrategies))
      (function (pickRandomWithWeight weightedList))
    )
    (function assocParamsLst)
  )
)

(define (keywordAnswer userResponse)
  (let* 
    (
      (chosenWord (pickRandom (findKeywords userResponse)))
      (chosenTemplate (pickRandom (findTemplates chosenWord)) )
      (response (manyReplace (list (list '* chosenWord)) chosenTemplate))
    )
    response
  )
)
			
; 1й способ генерации ответной реплики - замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifierAnswer userResponse)
  (append 
    (pickRandom 
      '(
        (you seem to think that)
        (you feel that)
        (why do you believe that)
        (why do you say that)
        (do you mean that)
        (so you are saying that)
        (why do you feel that)
      )
    )
    (changePerson userResponse)
  )
)

(define (historyAnswer oldPhrases) 
  (append '(earlier you said that) (changePerson (pickRandom oldPhrases)))
)

; случайный выбор одного из элементов списка lst
(define (pickRandom lst)
  (list-ref lst (random (length lst)))
)

; замена лица во фразе			
(define (changePerson phrase)
        (manyReplace '((am are)
                        (are am)
                        (i you)
                        (me you)
                        (mine yours)
                        (my your)
                        (myself yourself)
                        (you i)
                        (your my)
                        (yours mine)
                        (yourself myself))
                      phrase)
)
  
(define wordGroups '(
  (
    (depressed suicide exams university)
    (
      (when you feel depressed, go out for ice cream)
      (depression is a disease that can be treated)
      (depression can be defeated)
      (depression is for loosers!)
    )
  )
  (
    (mother father parents brother sister uncle ant grandma grandpa)
    (
      (tell me more about your * , i want to know all about your *)
      (why do you feel that way about your * ?)
      (i dont have *)
      (at least your * is alive!)
    )
  )
  (
    (university scheme lections)
    (
      (your education is important)
      (how many time do you spend to learning ?)
      (i guess, we are all talented in some way...)
      (nobody watches the diploma anyway)
    )
  )
  (
    (friend girlfriend wife husband)
    (
      (do you really need such * ?)
      (be careful, your * might be plotting against you...)
    )
  )
  (
    (money work debt loan)
    (
      (have you ever thought of becoming homeless? they are so carefree)
      (they killed my uncle because of his *)
    )
  )
))

(define keywords 
  (foldl
    (lambda (wg init)
      (append (car wg) init)
    )
    '()
    wordGroups
  )
)

(define (findTemplates word)
  (foldl 
    (lambda (wg init)
      (if (member word (car wg))
        (append (cadr wg) init)
        init
      )
    )
    '()
    wordGroups
  )
)

(define (findKeywords lst)
  (filter 
    (lambda (word)
      (member word keywords)
    )
    lst
  )
)

(define (hasKeywords lst)
  (ormap 
    (lambda (word) (member word keywords))
    lst
  )
)

(define (manyReplace replacementPairs lst)
  (map 
    (lambda (word)
      (let ((patRep (assoc word replacementPairs))) ; пара (ключ значение) или () ; Доктор ищет первый элемент списка в ассоциативном списке замен
        (if patRep (cadr patRep) word)
      )
    )
    lst
  )
)

; 2й способ генерации ответной реплики - случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge)
  (pickRandom 
    '(
      (please go on)
      (many people have the same sorts of feelings)
      (many of my patients have told me the same thing)
      (please continue)
      (intresting)
      (I understand you)
      (please tell more about it)
    )
  )
)

;=============== input/output ===============;


; (read-line)
;[.,:;()!?-] символы пунктуации

(define (parseString str)
  (define (removeBadChars str)
    (regexp-replace* #rx"([&\\^\\*\\+\\=\\_\\|\\/\\%\\$\\#\\№\\@\\>\\<\\`\\~\\{\\}\\(\\)\"]|[[]|[]]|\n)" str " " )
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
    (regexp-replace* #rx" *- *" 
      (regexp-replace* #rx" +([.,:;»)!?-])" str "\\1") ; убрал открывающую скобку
    "-")
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
    (println "readFileAsObject finished")
    data
  )
)

(define (writeFile data path)
  (let*
    (
      (outputPort (open-output-file path #:exists 'replace))
    )
    (print data outputPort)
    (close-output-port outputPort)
    "writeFile finished"
  )
)


; (hash-ref <hash> <key> <failval>)
; (hash-set! <hash> <key> <val>) 
; val: (prevs . nexts)

(define (isEnd? symbol)
  (or
    (equal? symbol '|.|)
    (equal? symbol '|?|)
    (equal? symbol '|!|)
  )
)

;=============== learning ===============;


(define (slidingWindow lst windowLength params fun)
  (if (< (length lst) windowLength)
    #f
    (let 
      (
        (thisVal (fun (take lst windowLength) params))
      )
      (if thisVal
        thisVal
        (slidingWindow (cdr lst) windowLength params fun)
      )
    )
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

(define (addKeyToHashTable hashTable symbol valIfNot)
  (let 
    (
      (value (hash-ref hashTable symbol #f))
    )
    (if value
      value
      (begin
        (hash-set! hashTable symbol valIfNot)
        valIfNot
      )
    )
  )
)

(define (learn starts.order N symbolList)
  (slidingWindow (cons '|.| symbolList) (+ N 1) starts.order 
    (lambda (n+1gram starts.order)
      (let*
        (
          (starts (car starts.order))
          (order (cdr starts.order))

          (prev (car n+1gram))
          (next (last n+1gram))
          (n-1gram (drop-right (cdr n+1gram) 1))

          (prevs.nexts (addKeyToHashTable order n-1gram (cons (make-hash) (make-hash))))
        )
        (if (isEnd? prev)
          (incCount starts n-1gram 1)
          "nothing"
        )
        (incCount (car prevs.nexts) prev 1)
        (incCount (cdr prevs.nexts) next 1)
        #f
      )
    )
  )
)

;=============== generating answer ===============;

(define (pickRandomKeyFromHT hashTable)
  (define sum (foldl
    (lambda (cur prev)
      (+ prev (hash-ref hashTable cur "noghing"))
    )
    0
    (hash-keys hashTable)
  ))
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


(define (generateAnswer starts.order userResponse)
  (define (wantEnd hashTable)
    (let 
      ((a (or
        (and (hash-ref hashTable '|.| #f) '|.|)
        (and (hash-ref hashTable '|!| #f) '|!|)
        (and (hash-ref hashTable '|?| #f) '|?|)
      )))
      (or a (pickRandomKeyFromHT hashTable))
    )
  )
  (define (forward order n-1gram lenLeft)
    ; (println n-1gram)
    (let* 
      (
        (nexts (cdr (hash-ref order n-1gram "nothing")))
        (next (if (<= lenLeft 0)
          (wantEnd nexts)
          (pickRandomKeyFromHT nexts)
        ))
      )
      (if (isEnd? next)
        (append n-1gram (list next))
        (cons (car n-1gram) (forward order (append (cdr n-1gram) (list next)) (- lenLeft 1)))
      )
    )
  )
  (define (backward order st lenLeft)
    (let* 
      (
        (prevs (car (hash-ref order st "nothing")))
        (prev (if (<= lenLeft 0)
          (wantEnd prevs)
          (pickRandomKeyFromHT prevs)
        ))
      )
      (if (isEnd? prev)
        st
        (append (backward order (cons prev (drop-right st 1)) (- lenLeft 1)) (last-pair st))
      )
    )
  )
  (define (mixed order st lenLeft)
    (let* 
      (
        (len (length st))
        (pre (drop-right (backward order st lenLeft) len))
        (post (list-tail (forward order st lenLeft) len))
      )
      (append pre st post)
    )
  )
  ; (define (slidingWindow lst windowLength params fun)
  (let* 
    (
      (starts (car starts.order))
      (order (cdr starts.order))
      (n-1 (length (car (hash-keys starts))))
      (stMatch (slidingWindow userResponse n-1 order (lambda (n-1gram order)
        (if (hash-ref order n-1gram #f)
          n-1gram
          #f
        )
      )))
      (lenLeft (random 15))
    )
    ; (println stMatch); NOTE: вывод совпадения n-1граммы из пользовательского ввода с текстом
    (if stMatch
      (mixed order stMatch lenLeft)
      (forward order (pickRandomKeyFromHT starts) (* 2 lenLeft))
    )
  )
)

(define (getFirstSentence lst)
  (if (null? lst)
    lst
    (if (isEnd? (car lst))
      (list (car lst))
      (cons (car lst) (getFirstSentence (cdr lst)))
    )
  )
)

(define (merge pair1 pair2)
  (define (mergeHashTables ht1 ht2)
    (hash-for-each ht2
      (lambda (key count)
        (incCount ht1 key count)
      )
    )
  )
  (mergeHashTables (car pair1) (car pair2))
  (hash-for-each (cdr pair2)
    (lambda (symbol prevs2.nexts2)
      (let*
        (
          (prevs1.nexts1 (addKeyToHashTable (cdr pair1) symbol (cons (make-hash) (make-hash))))
        )
        (mergeHashTables (car prevs1.nexts1) (car prevs2.nexts2))
        (mergeHashTables (cdr prevs1.nexts1) (cdr prevs2.nexts2))
      )
    )
  )
)


; ОБУЧАЕМСЯ
; (define bad (cons (make-hash) (make-hash)))
; (learn bad 2 (parseString (readFileAsString "_bad_tridon.txt")))
; (learn bad 2 (parseString (readFileAsString "_bad_jackson.txt")))
; (writeFile bad "backup_bad")

; (define good (cons (make-hash) (make-hash)))
; (learn good 4 (parseString (readFileAsString "_good_short_hilton.txt")))
; (learn good 4 (parseString (readFileAsString "_normal_sadger.txt")))
; (writeFile good "backup_good")

; ВСПОМИНАЕМ
(define good (readFileAsObject "backup_good"))
(define bad (readFileAsObject "backup_bad"))

; (define merged (cons (make-hash) (make-hash)))
; (merge merged good)
; (merge merged bad)

(visit-doctor 'stop 3 bad)


; pair: (
;   starts: hash(n-1gram, count),
;   order: hash(n-1gram, [hashPrevs, hashNexts])
; )