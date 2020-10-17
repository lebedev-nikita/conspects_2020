#lang scheme/base

(require scheme/mpair) 

(define (make-queue) (mcons 'queue '()))
(define (queue? q) (and (mpair? q) (eq? 'queue (mcar q))))
(define (empty-queue? q) (and (queue? q) (null? (mcdr q))))

; сложность вставки константная
(define (insert-queue! q e)
  (if (queue? q)
    (begin
      (set-mcdr! q (mcons e (mcdr q)))
      q
    )
    q
  )
)

(define (front-queue q)
  (if (not (queue? q))
    "not queue"
    (if (empty-queue? q)
      "empty queue"
      (mlist-ref q (- (mlength q) 1))
    )
  )
)

(define (delete-queue! q)  
  (define (deleteLast q)
    (if (null? (mcdr q))
      '()
      (mcons (mcar q) (deleteLast (mcdr q)))
    )
  )
  (begin
    (if (and (queue? q) (not (empty-queue? q)))
      (set-mcdr! q (deleteLast (mcdr q)))
      "it will not be returned anyway"
    )
    q
  )
)

;test
"(define testQueue (make-queue))"
(define testQueue (make-queue))
"(queue? testQueue)"
(queue? testQueue)
"(empty-queue? testQueue)"
(empty-queue? testQueue)
"(insert-queue! testQueue 1)"
(insert-queue! testQueue 1)
"(insert-queue! testQueue 2)"
(insert-queue! testQueue 2)
"(insert-queue! testQueue 3)"
(insert-queue! testQueue 3)
"(front-queue testQueue)"
(front-queue testQueue)
"(delete-queue! testQueue)"
(delete-queue! testQueue)
"(front-queue testQueue)"
(front-queue testQueue)
