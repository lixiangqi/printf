#lang racket

(provide node% edge%)

(define node%
  (class object%
    (field [x +nan.0]
           [y +nan.0]
           [px +nan.0]
           [py +nan.0]
           [weight 0.0]
           [fixed #f])
    
    (super-new)
    
    (define/public (get-x) x)
    (define/public (set-x new-x) (set! x new-x))
        
    (define/public (get-y) y)
    (define/public (set-y new-y) (set! y new-y))
        
    (define/public (get-px) px)
    (define/public (set-px new-px) (set! px new-px))
        
    (define/public (get-py) py)
    (define/public (set-py new-py) (set! py new-py))
        
    (define/public (fixed?) fixed)
    
    (define/public (get-weight) weight)))

(define edge%
  (class object%
    (init-field [from #f]
                [to #f]
                [label ""])
    
    (super-new)
    
    (define/public (get-from-node) from)
    (define/public (set-from-node f) (set! from f))
        
    (define/public (get-to-node) to)
    (define/public (set-to-node t) (set! to t))
    
    (define/public (get-label) label)))