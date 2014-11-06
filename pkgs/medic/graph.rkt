#lang racket

(require mrlib/graph
         racket/gui)

(define graph-pasteboard%
  (class (graph-pasteboard-mixin pasteboard%)
    (init-field [width 960.0]
                [height 800.0])
    (inherit get-snip-location)
    (field [friction 0.9]
           [charge -400.0]
           [gravity 0.1]
           [theta2 0.64]
           [link-distance 100.0]
           [link-strength 1.0]
           [charge-distance2 +inf.0]
           [alpha 0.1]
           [neighbors (make-hasheq)]
           [points (make-hasheq)]
           [lines (make-hasheq)]
           [node-labels (make-hasheq)]
           [edge-labels (make-hasheq)]
           [isDraging (make-hasheq)]
           [point-width 24.0]
           [point-height 24.0])
    (super-new)
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Layout function;;;;;;;;;;;;;;;
    
    (define edges (make-hasheq))
    (define nodes (make-hasheq))
    
    (define node%
      (class object%
        (init-field [graph #f])
        (field [x +nan.0]
               [y +nan.0]
               [px +nan.0]
               [py +nan.0]
               [weight 0.0])
        
        (define/public (get-x) x)
        (define/public (set-x new-x) (set! x new-x))
        
        (define/public (get-y) y)
        (define/public (set-y new-y) (set! y new-y))
        
        (define/public (get-px) px)
        (define/public (set-px new-px) (set! px new-px))
        
        (define/public (get-py) py)
        (define/public (set-py new-py) (set! py new-py))
        
        (define/public (get-weight) weight)
        
        (when graph
          (send graph install-node this))))
    
    (define edge%
      (class object%
        (field [from #f]
               [to #f]
               [label ""])
        
        (define/public (get-from-node) from)
        (define/public (set-from-node f) (set! from f))
        
        (define/public (get-to-node) to)
        (define/public (set-to-node t) (set! to t))
         
        
        ))
    
    (define/private (install-node n)
      (void))
    
    (define/private (install-edge e)
      (void))
    
    (define/private (uninstall-node n)
      (void))
    
    (define/private (uninstall-edge e)
      (void))
    
    (define/private (resume) (set! alpha 0.1))
    
    (define/private (tick)
      (set! alpha (* alpha 0.99))
      (if (< alpha 0.005)
          #f
          (let ([k 0.0]
                [x 0.0]
                [y 0.0])
            (for-each
             (lambda (e)
               (let* ([from (send e get-from-node)]
                      [to (send e get-to-node)]
                      [from-x (send from get-x)]
                      [from-y (send from get-y)]
                      [to-x (send to get-x)]
                      [to-y (send to get-y)])
                 (set! x (- to-x from-x))
                 (set! y (- to-y from-y))
                 (define l (+ (sqr x) (sqr y)))
                 (unless (zero? l))
                   (set! l (sqrt l))
                   (set! l (/ (* alpha link-strength (- l link-distance)) l))
                   (set! x (* x l))
                   (set! y (* y l))
                   (set! k (/ (send from get-weight) 
                                (+ (send to get-weight) (send from get-weight))))
                   (send to set-x (- to-x (* x k)))
                   (send to set-y (- to-y (* y k)))
                   (set! k (- 1 k))
                   (send from set-x (+ from-x (* x k)))
                   (send from set-y (+ from-y (* y k))))))
             (hash-values edges))
            (set! k (* alpha gravity))
            (unless (zero? k)
              (set! x (/ width 2.0))
              (set! y (/ height 2.0))
              (for-each
               (lambda (n)
                 (let ([n-x (send n get-x)]
                       [n-y (send n get-y)])
                 (send n set-x (+ n-x (* k (- x n-x))))
                 (send n set-y (+ n-y (* k (- y n-y))))))
               (hash-values nodes))
              
            
            
            
            
            )))
            
               
        
           
    ))