#lang scheme/base
(require racket/port)

(define (readFile path)
  (let* 
    (
      (inputPort (open-input-file path))
      (data (port->string inputPort))
    )
    (close-input-port inputPort)
    data
  )
)

(readFile "./io.scm")
