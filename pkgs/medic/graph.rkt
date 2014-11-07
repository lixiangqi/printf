#lang racket

(require mrlib/graph
         racket/gui
         "quadtree.rkt")

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
               [weight 0.0]
               [fixed #f])
        
        (define/public (get-x) x)
        (define/public (set-x new-x) (set! x new-x))
        
        (define/public (get-y) y)
        (define/public (set-y new-y) (set! y new-y))
        
        (define/public (get-px) px)
        (define/public (set-px new-px) (set! px new-px))
        
        (define/public (get-py) py)
        (define/public (set-py new-py) (set! py new-py))
        
        (define/public (fixed?) fixed)
        
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
                [y 0.0]
                [graph-nodes (hash-values nodes)])
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
                 (unless (zero? l)
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
               graph-nodes))
            (unless (zero? charge)
              (define qtree (new quadtree% [data graph-nodes]))
              (define qnode (send qtree get-root))
              (force-accumulate qnode)
              (for-each (lambda (n)
                          (unless (send n fixed?) (send qtree visit (repulse n))))
                        graph-nodes))
            (for-each 
             (lambda (n)
               (cond
                 [(send n fixed?)
                  (send n set-x (send n get-px))
                  (send n set-y (send n get-py))]
                 [else
                  (define node-x (send n get-x))
                  (define node-y (send n get-y))
                  (define temp-x node-x)
                  (define temp-y node-y)
                  (define node-px (send n get-px))
                  (define node-py (send n get-py))
                  (send n set-x (- node-x (* (- node-px node-x) friction)))
                  (send n set-px temp-x)
                  (send n set-y (- node-y (* (- node-py node-y) friction)))
                  (send n set-py temp-y)]))
             graph-nodes)
            #t)))
    
    (define/public (repulse node)
      (lambda (quad x1 y1 x2 y2)
        (define flag #f)
        (when (or (not (send quad get-point))
                         (not (eq? (send quad get-point) node)))
          (let* ([dx (- (send quad get-cx) (send node get-x))]
                 [dy (- (send quad get-cy) (send node get-y))]
                 [dw (- x2 x1)]
                 [dn (+ (sqr dx) (sqr dy))])
            (when (< (/ (sqr dw) theta2) dn)
              (when (< dn charge-distance2)
                (let ([k (/ (send quad get-charge) dn)])
                  (send node set-px (- (send node get-px) (* dx k)))
                  (send node get-py (- (send node get-py) (* dy k)))))
              (set! flag #t))
            (when (and flag (send quad get-point)
                       (not (zero? dn))
                       (< dn charge-distance2))
              (let ([k (/ (send quad get-point-charge) dn)])
                (send node set-px (- (send node get-px) (* dx k)))
                (send node get-py (- (send node get-py) (* dy k)))))))
        (or flag
            (< (send quad get-charge) 0.00005))))
    
    (define/public (force-accumulate quad)
      (define cx 0.0)
      (define cy 0.0)
      (send quad set-charge 0.0)
      (unless (send quad get-leaf)
        (let ([qt-nodes (send quad get-nodes)])
          (for-each
           (lambda (n)
             (when n
               (let ([node-charge (send n get-charge)])
                 (force-accumulate n)
                 (send quad set-charge (+ (send quad get-charge) node-charge))
                 (set! cx (+ cx (* node-charge (send n get-cx))))
                 (set! cy (+ cy (* node-charge (send n get-cy)))))))
           qt-nodes)))
      (when (send quad get-point)
        (define graph-node (send quad get-point))
        (when (not (send quad get-leaf))
          (send graph-node set-x (+ (send graph-node get-x) (- (random) 0.5)))
          (send graph-node set-y (+ (send graph-node get-y) (- (random) 0.5))))
        (let ([k (* alpha charge)])
          (send quad set-point-charge k)
          (send quad set-charge (+ (send quad get-charge) (send quad get-point-charge)))
          (set! cx (+ cx (* k (send graph-node get-x))))
          (set! cy (+ cy (* k (send graph-node get-y))))))
      (send quad set-cx (/ cx (send quad get-charge)))
      (send quad set-cy (/ cy (send quad get-charge))))
        
                 
        
      
          
        
                       


    
    
    
    
    ))