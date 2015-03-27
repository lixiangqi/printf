#lang racket

(define worker (thread
                 (lambda ()
                   (for ([i 10000])
                     (printf "Working hard... ~a~n" i)))))
(sleep 0.1)
(thread-wait worker)
(displayln "Worker finished")
(kill-thread worker)