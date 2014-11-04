#lang racket

(define graph%
  (class object%
    (init-field [width 960.0]
                [height 800.0])
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
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Layout function;;;;;;;;;;;;;;;
    
    (define edges (make-hasheq))
    (define nodes (make-hasheq))
    
    (define node%
      (class object%
        (init-field [graph #f])
        (field [x +nan.0]
               [y +nan.0]
               [px +nan.0]
               [py +nan.0])
        (when graph
          (send graph install-node this))))
    
    (define edge%
      (class object%
        (field [from #f]
               [to #f]
               [label ""])))
    
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
               
        
           
    ))