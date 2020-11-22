#lang scheme/base
(require racket/class)

(define 2tree<%> (interface () isEmpty? printTree))

(define Empty2Tree% 
  (class* object% (2tree<%>)
    (super-new)
    (define/public (isEmpty?) #t)
    (define/public (printTree) (printf ""))
  )
)

(define Nonempty2Tree%
  (class* object% (2tree<%>)
    (super-new)
    (init-field tag data)
    (field 
      (left (new Empty2Tree%))
      (right (new Empty2Tree%))
    )
    (define/public (isEmpty?) #f)
    (define/public (printTree)
      (send left printTree)
      (send right printTree)
      (println tag)
    )
    (define/public (get-tag) tag)
    (define/public (get-data) data)
    (define/public (set-tag t) (set! tag t))
    (define/public (set-data d) (set! data d))
    (define/public (get-left) left)
    (define/public (get-right) right)
    (define/public (set-left l) (if (is-a? l 2tree<%>) (set! left l) #f))
    (define/public (set-right r) (if (is-a? r 2tree<%>) (set! right r) #f))
  )
)

(define tt0 (new Nonempty2Tree% (tag 0) (data "data0")))
(define tt1 (new Nonempty2Tree% (tag 1) (data "data1")))
(define tt2 (new Nonempty2Tree% (tag 2) (data "data2")))
(send tt0 set-left tt1)
(send tt0 set-right tt2)
(send tt2 set-left "notTree")
(send tt0 printTree)

(define t (new Empty2Tree%))
(send t isEmpty?)
(send t printTree)
