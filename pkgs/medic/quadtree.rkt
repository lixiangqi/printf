#lang racket

(define quadtree-node%
  (class object%
    (field [leaf #t]
           [nodes (list null null null null)]
           [point #f]
           [x +nan.0]
           [y +nan.0]
           [charge +nan.0]
           [cx +nan.0]
           [cy +nan.0]
           [point-charge +nan.0])
    (define/public (get-leaf) leaf)
    
    (define/public (get-x) x)
    (define/public (set-x new-x) (set! x new-x))
    
    (define/public (get-y) y)
    (define/public (set-y new-y) (set! y new-y))
    
    (define/public (get-point) point)
    (define/public (set-point p) (set! point p))
    
    (super-new)))

(define quadtree%
  (class object%
    (init-field [data null])
    (field [x1 +inf.0]
           [y1 +inf.0]
           [x2 +inf.0]
           [y2 +inf.0]
           [xs null]
           [ys null]
           [root (new quadtree-node%)])
    
    (define/public (get-root) root)
    
    ;; Recursively inserts the specified point p at the Quadtree node n or one of its descendants
    ;; The bounds are specified by [x1, x2] and [y1, y2]
    ;; n: quadtree-node, v: graph node
    (define/public (insert n v x y x1 y1 x2 y2)
      (cond 
        [(or (nan? x) (nan? y)) #f]
        [(send n get-leaf)
         (define nx (send n get-x))
         (define ny (send n get-y))
         (if (nan? nx)
             (begin
               (send n set-x x)
               (send n set-y y)
               (send n set-point v))
             ; If the point at this leaf node is at the same position as the new
             ; point we are adding, we leave the point associated with the
             ; internal node while adding the new point to a child node. This
             ; avoids infinite recursion.
             (if (< (+ (abs (- nx x)) (abs (- ny y))) 0.01)
                 (insert-child n v x y x1 y1 x2 y2)
                 (let ([npoint (send n get-point)])
                   (send n set-point #f)
                   (send n set-x +nan.0)
                   (send n set-y +nan.0)
                   (insert-child n npoint nx ny x1 y1 x2 y2)
                   (insert-child n v x y x1 y1 x2 y2))))]
        [(not (send n get-leaf))
         (insert-child n v x y x1 y1 x2 y2)]))
    
    ;; Recursively inserts the specified point [x, y] into a descendant of node n.
    ;; The bounds are defined by [x1, x2] and [y1, y2].
    ;; n: quadtree-node, v: graph node
    (define/public (insert-child n v x y x1 y1 x2 y2)
      (define sx (* (+ x1 x2) 0.5))
      (define sy (* (+ y1 y2) 0.5))
      (define right (>= x sx))
      (define bottom (>= y sy))
      
      
         
        
      
    
    (super-new)
    (for-each
     (lambda (n)
       (let ([nx (send n get-x)]
             [ny (send n get-y)])
         (when (< nx x1) (set! x1 nx))
         (when (< ny y1) (set! y1 ny))
         (when (> nx x2) (set! x2 nx))
         (when (> ny y2) (set! y2 ny))
         (set! xs (append xs (list nx)))
         (set! ys (append ys (list ny)))))
     data)
    ; squarify bounds
    (define dx (- x2 x1))
    (define dy (- y2 y1))
    (if (> dx dy)
        (set! y2 (+ y1 dx))
        (set! x2 (+ x1 dy)))
    (for ([i (in-range (length data))])
      (insert root (list-ref data i) (list-ref xs i) (list-ref ys i) x1 y1 x2 y2))
    ; discard captured fields
    (set! xs null)
    (set! ys null)))
    
    
    
    
    
           
    
                