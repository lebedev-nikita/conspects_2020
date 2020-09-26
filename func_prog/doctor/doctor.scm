#lang scheme/base


; task 5
; параметр num - сколько еще пациентов доктор может принять
(define (visit-doctor stop-word num)
  (define (ask-patient-name)
    (begin
      (println '(next!))
      (println '(who are you?))
      (print '**)
      (car (read))
    ) 
  )
  (if (>= 0 num)
    '(time to go home)
    (let ((name (ask-patient-name)))
      (if (equal? name stop-word)
        '(working day finished)
        (begin
          (printf "Hello, ~a!\n" name)
          (print '(what seems to be the trouble?))
          (doctor-driver-loop name '())
          (visit-doctor stop-word (- num 1))
        )
      )
    )
  )
)

(define (doctor-driver-loop name old-phrases)
  (newline)
  (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
  (let ((user-response (read)))
    (cond 
      ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
            (printf "Goodbye, ~a!\n" name)
            (print '(see you next week))
            (newline)
      )
      (else (print (reply user-response old-phrases)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
            (doctor-driver-loop name (cons user-response old-phrases))
      )
    )
  )
)

; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response old-phrases)
  (let*
    (
      (flag2 (not (null? old-phrases)))
      (flag3 (has-keywords user-response))
      (num-variants (cond
        ((and flag2 flag3) 4)
        ((or flag2 flag3) 3)
        (else 2)
      ))
      (rnd (random num-variants))
    )
    (case rnd
      ((0) (qualifier-answer user-response)) ; 1й способ
      ((1) (hedge))  ; 2й способ
      ((2) (if flag2
        (history-answer old-phrases) ; 3й способ
        (keyword-answer user-response) ; 4й способ
      )) 
      ((3) (keyword-answer user-response)) ; 4й способ
    )
  )
)

(define strategies (
  (predicate weight function)
  (
    (lambda (assoc-param-list) #t) 
    1 
    (lambda (assoc-param-list)
      (let ((user-response (assoc 'user-response assoc-param-list)))
        (qualifier-answer user-response)
      )
    )
  )
  (
    (lambda (assoc-param-list) #t) 
    2 
    (lambda (assoc-param-list) (hedge))
  )
  (
    (lambda (assoc-param-list) (null? (assoc 'old-phrases assoc-param-list))) 
    3 
    (lambda (assoc-param-list)
      (let ((old-phrases (assoc 'old-phrases assoc-param-list)))
        (history-answer old-phrases)
      )
    )
  )
  (
    (lambda (assoc-param-list) 
      (let ((user-response (assoc 'user-response assoc-param-list)))
        (has-keywords user-response)
      )
    ) 
    4 
    (lambda (assoc-param-list)
      (let ((old-phrases (assoc 'old-phrases assoc-param-list)))
        (history-answer old-phrases)
      )
    )
  )
))

; task 7
(define (generalized-reply strategies assoc-params-lst)
  (let* 
    (
      (filtered-strategies (filter-by-predicate strategies assoc-params-lst))
      (weighted-list (map cdr filtered-strategies))
      (function (pick-random-with-weight weighted-list))
    )
    (function assoc-params-lst)
  )
)

(define (filter-by-predicate strategies param-vector)
  (filter  
    (lambda (strtg) 
      (let ((predicate (car strtg)))
      ; #t
        (predicate param-vector)
      )
    )
    strategies
  )
)


(define (pick-random-with-weight lst)
  (let* 
    (
      (max-random
        (foldl 
          (lambda (x init) (+ init (car x)))
          0
          lst
        )
      )
      (rand (random max-random))
    )
    (find-by-weight-and-rand lst rand)
  )
)


; lst: (weight function)

(define (find-by-weight-and-rand lst rand)
  (let* 
    (
      (this (car lst))
      (this-weight (car this))
      (this-function (cadr this))
    )
    (if (< rand this-weight)
      this-function
      (find-by-weight-and-rand (cdr lst) (- rand this-weight))
    )
  )
)

; ====== tests ====== 
(define (ttttest) 
  (filter-by-predicate 
    (list
      (list ; пример стратегии
        (lambda (param-vector) #t) ; предикат
        1 ; остальное
      )
      (list
        (lambda (param-vector) #t)
        2
      )
    )
    #()
  )
)

(define (tttest)
  (let ((lst (ttest 1000)))
    (list 
      (count 1 lst)
      (count 2 lst)
      (count 3 lst)
      (count 4 lst)
    )
  )
)

(define (count n lst) 
  (foldl 
    (lambda (x init)
      (if (= x n)
        (+ 1 init)
        init
      )
    )
    0
    lst
  )
)

(define (ttest n)
  (if (= 0 n)
    (cons (test) '())
    (cons (test) (ttest (- n 1)))
  )
)

(define (test) 
  (pick-random-with-weight '(
    (1 1)
    (2 2)
    (3 3)
    (4 4)
  ))
)

; (define (test n) 
;   (find-by-weight-and-rand 
;     '(
;       (1 1)
;       (2 2)
;       (3 3)
;       (4 4)
;     ) 
;     n
;   )
; )



; task 6
(define (keyword-answer user-response)
  (let* 
    (
      (chosen-word (pick-random (find-keywords user-response)))
      (chosen-template (pick-random (find-templates chosen-word)) )
      (response (many-replace (list (list '* chosen-word)) chosen-template))
    )
    response
  )
)
			
; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response)
  (append 
    (pick-random 
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
    (change-person user-response)
  )
)

(define (history-answer old-phrases) 
  (append '(earlier you said that) (change-person (pick-random old-phrases)))
)

; случайный выбор одного из элементов списка lst
(define (pick-random lst)
  (list-ref lst (random (length lst)))
)

; замена лица во фразе			
(define (change-person phrase)
        (many-replace '((am are)
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
  
(define word-groups '(
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
    word-groups
  )
)

(define (find-templates word)
  (foldl 
    (lambda (wg init)
      (if (member word (car wg))
        (append (cadr wg) init)
        init
      )
    )
    '()
    word-groups
  )
)

(define (find-keywords lst)
  (filter 
    (lambda (word)
      (member word keywords)
    )
    lst
  )
)

(define (has-keywords lst)
  (ormap 
    (lambda (word) (member word keywords))
    lst
  )
)

(define (many-replace replacement-pairs lst)
  (map 
    (lambda (word)
      (let ((pat-rep (assoc word replacement-pairs))) ; пара (ключ значение) или () ; Доктор ищет первый элемент списка в ассоциативном списке замен
        (if pat-rep (cadr pat-rep) word)
      )
    )
    lst
  )
)

; 2й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge)
  (pick-random 
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
