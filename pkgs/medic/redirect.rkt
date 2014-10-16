#lang racket

(provide set-up-output-port
         add-layer-id
         process-logs
         access-layer
         access-logs)

(define output-port (open-output-string))
(define layers '())
(define logs '())
  
(define (set-up-output-port) output-port)

(define (add-layer-id id) 
  (set! layers (append layers (list id))))

(define (process-logs)
  (define in (open-input-string (get-output-string output-port)))
  (let iterate ([line (read-line in)])
    (unless (eof-object? line)
      (set! logs (append logs (list line)))
      (iterate (read-line in))))
  (close-input-port in)
  (close-output-port output-port))

(define (access-logs) logs)

(define (access-layer layer-id)
  (define res '())
  (for ([i (in-range (length layers))])
    (when (equal? (list-ref layers i) layer-id)
      (set! res (append res (list (list-ref logs i))))))
  res)
  