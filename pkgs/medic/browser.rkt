#lang racket

(require racket/class
         racket/gui/base
         framework
         "graph-pasteboard.rkt"
         "graph-layout.rkt"
         "edge.rkt")

(provide make-trace-browser)

  
(define (make-trace-browser fn)
  (define frame (new trace-frame% [filename fn]))
  (send frame show #t))

(define trace-frame%
  (class (frame:basic-mixin frame%)
    (init-field (filename #f))
    (inherit get-area-container)
    (super-new (label (make-label))
               (width 800)
               (height 600))
    (define widget (new widget% [parent (get-area-container)]))
    
    (define/private (make-label)
      (if filename
          (string-append filename " - Traces")
          "Traces"))))

(define widget%
  (class object%
    (init parent)
    (field [nodes #f]
           [edges #f])
           
    (super-new)
    
    (define split-panel (new panel:vertical-dragable% [parent parent]))
    (define log-panel (new vertical-panel% [parent split-panel]))
    (define timeline-panel (new vertical-panel% [parent split-panel]))
    (define graph-panel (new vertical-panel% [parent split-panel]))
    
    (define graph-pb (new graph-pasteboard%))
    
    (new editor-canvas% 
         [parent log-panel]
         [style '(auto-hscroll)])
    (new canvas%
         [parent timeline-panel]
         [style '(hscroll)])
    (new editor-canvas%
         [parent graph-panel]
         [editor graph-pb])
    
    #;(define/public (update-graph-info n e)
      (set! nodes n)
      (set! edges e))
      
    
         
         
      
    
    (send split-panel begin-container-sequence)
    (send split-panel set-percentages (list 1/3 1/3 1/3))
    (send split-panel end-container-sequence)
    ))
    