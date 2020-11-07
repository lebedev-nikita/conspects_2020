#lang scheme/base
(require racket/stream)

(define (streamMergeSorted stream1 stream2)
  (if (< (stream-first stream1) (stream-first stream2))
    (stream-cons 
      (stream-first stream1)
      (streamMergeSorted 
        (stream-rest stream1)
        stream2
      )
    )
    (stream-cons
      (stream-first stream2)
      (streamMergeSorted 
        stream1
        (stream-rest stream2)
      )
    )
  )
)

(define (powers x)
  (let pow-gen ((a 1) (b x))
    (stream-cons a (pow-gen (* a b) b))
  )
)

(define (printStream stream len)
  (if (= len 1)
    (println (stream-first stream))
    (begin
      (println (stream-first stream))
      (printStream (stream-rest stream) (- len 1))
    )
  )
)

(printStream (streamMergeSorted (stream-rest (powers 5)) (powers 3)) 10)