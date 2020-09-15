; заготовка "Доктора". Август 2019
#lang scheme/base
; В учебных целях используется базовая версия Scheme

; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
                           ; task 4
  (doctor-driver-loop name '())
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
             (print '(see you next week)))
            (else (print (reply user-response old-phrases)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop name (cons user-response old-phrases))
            )
      )
    )
)

; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response old-phrases)
  ; task 4
  (if (null? old-phrases)
      (case (random 2) ; с равной вероятностью выбирается один из двух способов построения ответа
          ((0) (qualifier-answer user-response)) ; 1й способ
          ((1) (hedge))  ; 2й способ
      )
      (case (random 3) ; с равной вероятностью выбирается один из двух способов построения ответа
          ((0) (qualifier-answer user-response)) ; 1й способ
          ((1) (hedge))  ; 2й способ
          ((2) (history-answer old-phrases))
      )
  )
)
			
; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response)
        (append (pick-random '((you seem to think that)
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
  (append '(earlier you said that) (pick-random old-phrases))
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
  
(define (test) (qualifier-answer '(i feel bored)))

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

; ; task 2
; (define (many-replace replacement-pairs lst)
;   (let 
;     ((reversed-replaced 
;       (let rec-replace 
;         (
;           (left '())
;           (right lst)
;         )
;         (if (null? right)
;           left
;           (let ((pat-rep (assoc (car right) replacement-pairs))) ; пара (ключ значение) или () ) ; Доктор ищет первый элемент списка в ассоциативном списке замен
;             (let ((new-val (if pat-rep (cadr pat-rep) (car right)))) 
;               (rec-replace (cons new-val left) (cdr right))
;             )
;           )
;         )
;       )
;     ))
;     (let rec-reverse 
;       (
;         (left '())
;         (right reversed-replaced)
;       )
;       (if (null? right)
;         left
;         (rec-reverse (cons (car right) left) (cdr right))
;       )
;     )
;   )
; )

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
