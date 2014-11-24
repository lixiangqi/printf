#lang racket

(require racket/class
         racket/gui/base
         framework
         "log-text.rkt"
         "graph-pasteboard.rkt"
         "timeline-canvas.rkt"
         "visual-util.rkt")

(provide make-trace-browser)

  
(define (make-trace-browser fn)
  (define frame (new trace-frame% [filename fn]))
  (send frame show #t))

(define trace-frame%
  (class (frame:basic-mixin frame%)
    (init-field (filename #f))
    (inherit get-area-container)
    (super-new (label (make-label))
               (width 1200)
               (height 800))
    (define widget (new widget% [parent (get-area-container)]))
    
    (define/private (make-label)
      (if filename
          (string-append filename " - Traces")
          "Traces"))))

(define widget%
  (class object%
    (init parent)
           
    (super-new)
   
    (define split-panel (new panel:horizontal-dragable% [parent parent]))
    (define log-panel (new vertical-panel% [parent split-panel]))
    (define right-panel (new panel:vertical-dragable% [parent split-panel]))
    (define top-right-panel (new panel:horizontal-dragable% [parent right-panel]))
    (define graph-panel (new vertical-panel% [parent top-right-panel]))
    (define aggre-panel (new vertical-panel% [parent top-right-panel]))
    (define timeline-panel (new vertical-panel% [parent right-panel]))
    
    (send split-panel begin-container-sequence)
    (send split-panel set-percentages (list 1/4 3/4))
    (send split-panel end-container-sequence)
    (send right-panel begin-container-sequence)
    (send right-panel set-percentages (list 1/2 1/2))
    (send right-panel end-container-sequence)
    (send top-right-panel begin-container-sequence)
    (send top-right-panel set-percentages (list 1/2 1/2))
    (send top-right-panel end-container-sequence)
    
    
    (define timeline-data (get-timeline-data))
    (set! timeline-data 
          (list 
           (list "str1" #f (list "hello" "world" 1 #t "time line" "core" "canvas" "to" 'started 'loop 'download 'it 6 9 #t #f
                                "hello" "world" 1 #t "time line" "core" "canvas" "to" 'started 'loop 'download 'it 6 9 #t #f))
           (list "x1" #f (list 3 7 5 9 10))
           (list "y1" #f (list -1 2 3 -4 5))
           (list "z1" #f (list 5 5 5 5 5))
           (list "x1" #f (list #f #f #f #f #t))
           (list "x1 > 5" #t (list #f #t #f #t #t))
           
           (list "str2" #f (list "hello" "world" 1 #t "time line" "core" "canvas" "to" 'started 'loop 'download 'it 6 9 #t #f
                                "hello" "world" 1 #t "time line" "core" "canvas" "to" 'started 'loop 'download 'it 6 9 #t #f))
           (list "x2" #f (list 3 7 5 9 10))
           (list "y2" #f (list -1 2 3 -4 5))
           (list "z2" #f (list 5 5 5 5 5))
           (list "x2" #f (list #f #f #f #f #t))
           (list "x2 > 5" #t (list #f #t #f #t #t))
           ))
    (define max-length (apply max (map length (map third timeline-data))))
    
    (define slider-panel (new horizontal-panel% 
                              [parent timeline-panel] 
                              [stretchable-height #f]
                              [style '(border)]))
    (define prefix "Timeline\n")
    (define slider-message (new message% 
                                [label (string-append prefix (format "~a/~a" 0 max-length))] 
                                [parent slider-panel]))
    
    (define (on-step slider event)
      (define cur (send slider get-value))
      (send slider-message set-label (string-append prefix (format "~a/~a" cur max-length)))
      (send timeline-canvas scrutinize cur))
    
    (define slider (new slider% 
                        [label ""] 
                        [min-value 0] 
                        [max-value max-length] 
                        [parent slider-panel]
                        [callback on-step]))
    
    (define-values (graph-width graph-height) (send graph-panel get-size))
    (define graph-pb (new graph-pasteboard%
                          [raw-edges (get-raw-edges)]
                          [width graph-width]
                          [height graph-height]))
    
    (define log-text (new log-text%))
    
    (new message% [parent log-panel] [label "Log"])
    
    (new editor-canvas% 
         [parent log-panel]
         [editor log-text]
         [style '(no-focus auto-hscroll auto-vscroll)])
    
    
    
    (define timeline-canvas (new timeline-canvas%
                                 [data timeline-data]
                                 [parent timeline-panel]
                                 [style '(hscroll vscroll)]))
    
    (send timeline-canvas init-auto-scrollbars 
          (send timeline-canvas get-actual-width) 
          (send timeline-canvas get-actual-height)
          0.0 0.0)
    (send timeline-canvas show-scrollbars #t #t)
    (new message% [parent graph-panel] [label "Graph"])
    (new editor-canvas%
         [parent graph-panel]
         [editor graph-pb])
    (new message% [parent aggre-panel] [label "Aggregate"])
    (new editor-canvas%
         [parent aggre-panel])
      
    
    
    ))
    