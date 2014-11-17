#lang racket

(require racket/gui/base)

(provide timeline-canvas%)

(define timeline-canvas%
  (class canvas%
    (init-field (data #f))
    (inherit get-dc)
    (super-new)
    
    (printf "data=~v\n" data)
    (define labels (map first data))
    (define asserts (map second data))
    (define values (map third data))
    (define dc (get-dc))
    
    (define (convert-to-type l)
      (define (is-number-series? l)
        (andmap (lambda (b) (eq? b #t)) (map number? l)))
      (define (is-boolean-series? l)
        (andmap (lambda (b) (eq? b #t)) (map boolean? l)))
      (cond
        [(is-number-series? l) 'number]
        [(is-boolean-series? l) 'boolean]
        [else 'other]))
    
    (define types (map convert-to-type values))
    
    (define timeline-space 50)
    (define max-width (apply max (map (lambda (s) (car (get-text-size s))) labels)))
    (define start-x #f)
    (define start-y #f)
    
    (define square-size 50)
    (define square-center (/ square-size 2))
    (define text-height (cdr (get-text-size "test")))
    (define bitmap-width (inexact->exact (- square-size 4)))
    (define bitmap-height (inexact->exact text-height))
    (define offset (- square-center bitmap-height))
    
    (define/private (get-text-size str)
      (define dc (new bitmap-dc% [bitmap (make-object bitmap% 1 1)]))
      (define-values (w h r1 r2) (send dc get-text-extent str))
      (cons w h))
    
    (define/private (get-text-width s)
      (define-values (w h r1 r2) (send dc get-text-extent s))
      w)
    
    (define ellipsis-offset (- square-center (/ (get-text-width "...") 2)))
    
    (define/private (visualize-number l)
      
      (define lower-bound (inexact->exact (floor (apply min l))))
      (define upper-bound (inexact->exact (ceiling (apply max l))))
      (define units (- upper-bound lower-bound))
      (define space 6)
      (define ellipse-width 6)
      (define radius (/ ellipse-width 2))
      
      (define (get-line-position x1 y1 x2 y2)
        (define theta (atan (- y2 y1) (- x2 x1)))
        (define sint (sin theta))
        (define cost (cos theta))
        (define m (* radius cost))
        (define n (* radius sint))
        (define line-x1 (+ x1 m))
        (define line-y1 (+ y1 n))
        (define line-x2 (- x2 m))
        (define line-y2 (- y2 n))
        (values line-x1 line-y1 line-x2 line-y2))
      
      (unless (zero? units)
      (define unit-length (/ (- square-size space space) units))
      (define len (length l))
      
        (send dc set-brush "LightGray" 'solid)
      (let loop ([i 0])
        (when (< i len)
          (let* ([val (list-ref l i)]
                 [plot-height (* (- val lower-bound) unit-length)]
                 [ellipse-x (- (+ start-x (* square-size i) square-center) radius)]
                 [ellipse-y (- (+ start-y (- square-size space plot-height)) radius)])
            (send dc set-pen "White" 0 'solid)
            (send dc draw-rectangle (+ start-x (* square-size i)) start-y square-size square-size)
            (send dc set-pen "DodgerBlue" 1 'solid)
            (send dc draw-ellipse ellipse-x ellipse-y ellipse-width ellipse-width)
            (loop (add1 i)))))
        
        (send dc set-pen "White" 0 'solid)
        ))
    
    (define/private (visualize-boolean l true-color false-color)
      (define len (length l))
      (let loop ([i 0])
        (when (< i len)
          (if (list-ref l i)
              (send dc set-brush true-color 'solid)
              (send dc set-brush false-color 'solid))
          (send dc draw-rectangle (+ start-x (* square-size i)) start-y square-size square-size)
          (loop (add1 i)))))
   
    (define/private (visualize-other-data l)
      (define len (length l))
      (define bm-start-x (+ start-x 2))
      (define bm-start-y (+ start-y offset))
      (define ellipsis-start-x (+ start-x ellipsis-offset))
      (define ellipsis-start-y (+ start-y square-center))
      (let loop ([i 0])
        (when (< i len)
          (let ([value (format "~v" (list-ref l i))]) ; (* square-size i)
          (send dc set-brush "LightGray" 'solid)
          (send dc draw-rectangle (+ start-x (* square-size i)) start-y square-size square-size)
          (define bm (make-object bitmap% bitmap-width bitmap-height #f #t))
          (define bm-dc (new bitmap-dc% [bitmap bm]))
          
          (send bm-dc draw-text value 0 0)
          (send dc draw-bitmap bm (+ bm-start-x (* square-size i)) bm-start-y)
          (when (> (car (get-text-size value)) (add1 bitmap-width)) 
            (send dc draw-text "..." (+ ellipsis-start-x (* square-size i)) ellipsis-start-y))
          (loop (add1 i))))))
    
    (define/private (draw-labels)
      (send dc set-text-foreground "Gray")
      (define max-label-width (apply max (map (lambda (s) (get-text-width s)) labels)))
      (set! start-x (+ 2 max-label-width))
      (define len (length labels))
      (let loop ([i 0])
        (when (< i len)
          (send dc draw-text (list-ref labels i) 2 (+ square-size offset (* square-size i)))
          (loop (add1 i)))))
    
    (define/private (draw-frame-number)
      (define max-number (apply max (map length values)))
      (let loop ([i 0])
        (when (< i max-number)
          (send dc draw-text (format "~a" (add1 i)) (+ start-x 2 (* square-size i)) square-center)
          (loop (add1 i)))))
      
    (define/override (on-paint)
      (set! start-x max-width)
      (set! start-y square-size)
      
      (draw-labels)
      (draw-frame-number)
      (send dc set-text-foreground "Black")
      (send dc set-pen "White" 0 'solid)
      (for ([i (in-range (length types))])
        (let ([t (list-ref types i)]
              [d (list-ref values i)]
              [assert? (list-ref asserts i)])
          (case t
            [(number) (visualize-number d)]
            [(boolean) (if assert? (visualize-boolean d "LightGray" "Red") (visualize-boolean d "Blue" "Red"))]
            [(other) (visualize-other-data d)])
          (set! start-y (+ start-y square-size))))
                       
      
      
      
      
      
      
      ;(send dc set-pen "White" 0 'solid)
      ;(visualize-boolean (cdr (second data)))
      ;(visualize-other-data (cdr (third data)))
      
      
;      (send dc set-brush "Firebrick" 'solid)
;      
;      (send dc draw-rectangle start start square-size square-size)
;      (send dc set-brush "ForestGreen" 'solid)
;      (send dc draw-rectangle (+ square-size start) start square-size square-size)
;      
;      (define bm (make-object bitmap% (- square-size 2) 25 #f #t))
;      (define bm-dc (new bitmap-dc% [bitmap bm]))
;      (send bm-dc draw-text "hellohellohello" 0 0)
;      (send dc draw-bitmap bm start start)
;      (send dc draw-text "..." 70 80)
;      
;      #;(let loop ([i 0])
;        (when (< i 10)
;          (send dc draw-rectangle (+ start (* square-size i)) start square-size square-size)
;          (loop (add1 i))))
;      ;(send dc draw-line 0 50 4000 50)
      #;(send dc draw-rectangle 3948 60 50 50))
      
      
    
    ))