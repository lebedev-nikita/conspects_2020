#lang scheme/base

; параметр num - сколько еще пациентов доктор может принять
(define (visit-doctor stopWord num)
  (define (askPatientName)
    (begin
      (println '(next!))
      (println '(who are you?))
      (print '**)
      (car (read))
    ) 
  )
  (if (>= 0 num)
    '(time to go home)
    (let ((name (askPatientName)))
      (if (equal? name stopWord)
        '(working day finished)
        (begin
          (printf "Hello, ~a!\n" name)
          (print '(what seems to be the trouble?))
          (doctorDriverLoop name '())
          (visit-doctor stopWord (- num 1))
        )
      )
    )
  )
)

(define (t) (doctorDriverLoop 'Nikita '()))

(define (doctorDriverLoop name oldPhrases)
  (newline)
  (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
  (let ((userResponse (read)))
    (cond 
      ((equal? userResponse '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
            (printf "Goodbye, ~a!\n" name)
            (print '(see you next week))
            (newline)
      )
      (else 
        (print 
          (generalizedReply strategies (list
              (list 'userResponse userResponse) 
              (list 'oldPhrases oldPhrases)
          ))
        ) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
        (doctorDriverLoop name (cons userResponse oldPhrases))
      )
      ; (else (print (reply userResponse oldPhrases)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
      ;       (doctorDriverLoop name (cons userResponse oldPhrases))
      ; )
    )
  )
)

; генерация ответной реплики по userResponse - реплике от пользователя 
(define (reply userResponse oldPhrases)
  (let*
    (
      (flag2 (not (null? oldPhrases)))
      (flag3 (hasKeywords userResponse))
      (numVariants (cond
        ((and flag2 flag3) 4)
        ((or flag2 flag3) 3)
        (else 2)
      ))
      (rnd (random numVariants))
    )
    (case rnd
      ((0) (qualifierAnswer userResponse)) ; 1й способ
      ((1) (hedge))  ; 2й способ
      ((2) (if flag2
        (historyAnswer oldPhrases) ; 3й способ
        (keywordAnswer userResponse) ; 4й способ
      )) 
      ((3) (keywordAnswer userResponse)) ; 4й способ
    )
  )
)

; task 7
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
))

; task 7
(define (generalizedReply strategies assocParamsLst)
  (define (filterByPredicate strategies assocParamList)
    (filter  
      (lambda (strtg) 
        (let ((predicate (car strtg)))
        ; #t
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
