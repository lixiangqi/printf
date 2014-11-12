#lang racket

(require racket/gui/base)

(provide timeline-canvas%)

(define timeline-canvas%
  (class canvas%
    (inherit get-dc)
    (super-new)
    
    (define data (list (cons "x" (list 3 7 5 9 10))
                       (cons "x > 5" (list #f #t #f #t #t))
                       (cons "str" (list "hello" "world"))))
    
    (define timeline-space 50)
    (define dc (get-dc))
    (define/override (on-paint)
      (send dc draw-line 50 50 4500 50))
      
    
    ))