#lang scheme/base


; task 5
; параметр num - сколько еще пациентов доктор может принять
(define (visit-doctor stop-word num)
  (if (< 0 num)
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
    '(time to go home)
  )
)

(define (ask-patient-name)
  (begin
    (println '(next!))
    (println '(who are you?))
    (print '**)
    (car (read))
  ) 
)
; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
                                 ; task 4
(define (doctor-driver-loop name old-phrases)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week))
             (newline))
            (else (print (reply user-response old-phrases)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop name (cons user-response old-phrases))
            )
      )
    )
)

; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response old-phrases)
  ; task 4
  (let* 
    (
      (min-variants 2)
      (flag1 (if (null? old-phrases) 0 1))
      (flag2 (if (has-keywords user-response keywords) 1 0))
      (num-variants (+ min-variants flag1 flag2))
    )
    (case (random num-variants)
      ((0) (qualifier-answer user-response)) ; 1й способ
      ((1) (hedge))  ; 2й способ
      ((2) (kw-answer user-response keywords)) ; 4й способ
      ((3) (history-answer old-phrases)) ; 3й способ
    )
  )
)

; task 6
(define (kw-answer user-response word-groups)
  (let* 
    (
      (match-words (find-keywords user-response word-groups))
      (chosen-word (pick-random match-words))
      (match-groups (find-groups chosen-word word-groups) )
      (chosen-group (pick-random match-groups))
      (chosen-template (pick-random (cadr chosen-group)))
      (response (many-replace (list (list '* chosen-word)) chosen-template))
    )
    response
  )
)

(define (test) (kw-answer '(i am depressed) keywords))
			
; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response)
        (append (pick-random '(
                                (you seem to think that)
                                (you feel that)
                                (why do you believe that)
                                (why do you say that)
                                ; task 1
                                (do you mean that)
                                (so you are saying that)
                                (why do you feel that)
                              )
                )
                (change-person user-response)
        )
)

; task 4
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
  
(define keywords '(
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
      (i dont have a *)
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
      (they killed my uncle because of *)
    )
  )
))

(define (includes lst word)
  (ormap 
    (lambda (elem) (equal? elem word))
    lst
  )
)

(define (find-groups word word-groups)
  (filter 
    (lambda (wg)
      (includes (car wg) word)
    )
    word-groups
  )
)

(define (find-keywords lst word-groups)
  (filter 
    (lambda (word)
      (not (null? (find-groups word word-groups) ))
    )
    lst
  )
)

(define (has-keywords lst word-groups)
  (not (null? (find-keywords lst word-groups)))
)


; task 3
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
        (pick-random '((please go on)
                       (many people have the same sorts of feelings)
                       (many of my patients have told me the same thing)
                       (please continue)
                       ; task 1
                       (intresting)
                       (I understand you)
                       (please tell more about it)
                      )
        )
)
