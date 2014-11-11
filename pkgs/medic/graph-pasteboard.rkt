#lang racket

(require mrlib/graph
         racket/gui/base
         "quadtree.rkt"
         "visual-util.rkt")

(provide graph-pasteboard%
         graph-image-snip%)

(define snip-size 30)

(define (create-node-bitmap)
  (define bm (make-object bitmap% snip-size snip-size))
  (define bm-dc (new bitmap-dc% [bitmap bm]))
  (send bm-dc set-brush "blue" 'solid)
  (send bm-dc draw-ellipse 0 0 snip-size snip-size)
  bm)

(define graph-image-snip%
  (class (graph-snip-mixin image-snip%)
    (field [label #f])
    (super-new)
    
    (define/public (set-label l) (set! label l))
    
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (send dc draw-text label (+ x snip-size) (+ y snip-size))
      (super draw dc x y left top right bottom dx dy draw-caret))))


(define graph-pasteboard%
  (class (graph-pasteboard-mixin pasteboard%)
    (init-field [width #f]
                [height #f])
    
    (define friction 0.9)
    (define charge -400.0)
    (define gravity 0.1)
    (define theta2 0.64)
    (define link-distance 100.0)
    (define link-strength 1.0)
    (define charge-distance2 +inf.0)
    (define alpha 0.1)
    (define neighbors (make-hash))
    
    (define snips (make-hash))
    
    (define lines (make-hasheq))
    (define node-labels (make-hash))
    (define edge-labels (make-hash))
    (define isDraging (make-hash))
    (define point-width 24.0)
    (define point-height 24.0)
    
    (define nodes (make-hash))
    (define edges (make-hash))
    
    (define/private (init-graph-elements)
      (define raw-edges (get-raw-edges))
      (for-each
       (lambda (key)
         (let* ([from (car key)]
                [to (cdr key)]
                [val (hash-ref raw-edges key)]
                [edge-label (first val)]
                [from-label (second val)]
                [to-label (third val)])
           (unless (hash-has-key? nodes from) (hash-set! nodes from (new node% [label from-label])))
           (unless (hash-has-key? nodes to) (hash-set! nodes to (new node% [label to-label])))
           (define from-node (hash-ref nodes from))
           (define to-node (hash-ref nodes to))
           (unless (equal? from-node to-node)
             (let ([k (cons from to)])
               (unless (hash-has-key? edges k) 
                 (hash-set! edges k (new edge% [from from-node] [to to-node] [label edge-label])))))))
         (hash-keys raw-edges)))
      
   
    (define node%
      (class object%
        (init-field [label #f])
        (field [x +nan.0]
               [y +nan.0]
               [px +nan.0]
               [py +nan.0]
               [weight 0.0]
               [fixed #f])
        (super-new)
        (install-node this)
        
        (define/public (get-x) x)
        (define/public (set-x new-x) (set! x new-x))
        
        (define/public (get-y) y)
        (define/public (set-y new-y) (set! y new-y))
        
        (define/public (get-px) px)
        (define/public (set-px new-px) (set! px new-px))
        
        (define/public (get-py) py)
        (define/public (set-py new-py) (set! py new-py))
        
        (define/public (fixed?) fixed)
        (define/public (get-label) label)
        (define/public (get-weight) weight)
        (define/public (incr-weight) (set! weight (add1 weight)))))
    
    (define edge%
      (class object%
        (init-field [from #f]
                    [to #f]
                    [label ""])
    
        (super-new)
        (install-edge this)
    
        (define/public (get-from-node) from)
        (define/public (set-from-node f) (set! from f))
        
        (define/public (get-to-node) to)
        (define/public (set-to-node t) (set! to t))
    
        (define/public (get-label) label)))
    
    (define/private (install-node n)
      (when (nan? (send n get-x)) (send n set-x (position n 0)))
      (when (nan? (send n get-y)) (send n set-y (position n 1)))
      (when (nan? (send n get-px)) (send n set-px (send n get-x)))
      (when (nan? (send n get-py)) (send n set-py (send n get-y)))
      (define s (make-object graph-image-snip% (create-node-bitmap)))
      (send s set-label (send n get-label))
      (hash-set! snips n s))
    
    (define/private (install-edge e)
      (define from (send e get-from-node))
      (define to (send e get-to-node))
      (send from incr-weight)
      (send to incr-weight)
      (if (hash-has-key? neighbors from)
          (hash-set! neighbors from (append (hash-ref neighbors from) (list to)))
          (hash-set! neighbors from (list to)))
      (if (hash-has-key? neighbors to)
          (hash-set! neighbors to (append (hash-ref neighbors to) (list from)))
          (hash-set! neighbors to (list from))))
    
    (define/private (uninstall-node n)
      (void))
    
    (define/private (uninstall-edge e)
      (void))
    
    (define/public (update-nodes)
      (tick)
      (for-each
       (lambda (n)
         (printf "node x=~v, y=~v\n" (send n get-x) (send n get-y)))
       (hash-values nodes))
      (void))
    
    (define/public (update-edges)
      (void))
    
    (define/public (resume) (set! alpha 0.1))
    
    (define/public (tick)
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
    
    ;; Supports stable incremental layout. When a new node is added and if a linked node
    ;; already has an initial position, the corresponding coordinates are applied
    ;; to the new node, rather than generating random position.
    (define/private (position n d)
      (define ret #f)
      (when (hash-has-key? neighbors n)
        (for-each 
         (lambda (node)
           (let ([n-x (send node get-x)]
                 [n-y (send node get-y)])
             (when (and (zero? d) (not (nan? n-x))) (set! ret n-x))
             (when (and (not ret) (= d 1) (not (nan? n-y))) (set! ret n-y))))
         (hash-ref neighbors n)))
      (or ret (* (random) (if (zero? d) width height)))) 
      
    
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
                  (send node set-py (- (send node get-py) (* dy k)))))
              (set! flag #t))
            (when (and flag (send quad get-point)
                       (not (zero? dn))
                       (< dn charge-distance2))
              (let ([k (/ (send quad get-point-charge) dn)])
                (send node set-px (- (send node get-px) (* dx k)))
                (send node set-py (- (send node get-py) (* dy k)))))))
        (or flag
            (< (send quad get-charge) 0.00005))))
    
    (define/public (force-accumulate quad)
      (define cx 0.0)
      (define cy 0.0)
      (send quad set-charge 0.0)
      (unless (send quad get-leaf)
        (let ([qt-nodes (send quad get-nodes)])
          (for ([i (in-range (vector-length qt-nodes))])
            (let ([n (vector-ref qt-nodes i)])
              (when n
                (let ([node-charge (send n get-charge)])
                  (force-accumulate n)
                  (send quad set-charge (+ (send quad get-charge) node-charge))
                  (set! cx (+ cx (* node-charge (send n get-cx))))
                  (set! cy (+ cy (* node-charge (send n get-cy))))))))))
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
    
    (super-new)
    (init-graph-elements)
    (update-nodes)
    ))