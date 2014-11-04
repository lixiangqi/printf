#lang racket

(require framework
         racket/gui/base)

(provide widget%)

(define widget%
  (class object%
    (init parent)
    (super-new)
    
    (define split-panel (new panel:vertical-dragable% [parent parent]))
    (define log-panel (new vertical-panel% [parent split-panel]))
    (define timeline-panel (new vertical-panel% [parent split-panel]))
    (define graph-panel (new vertical-panel% [parent split-panel]))
    
    (new editor-canvas% 
         [parent log-panel]
         [style '(auto-hscroll)])
    (new canvas%
         [parent timeline-panel]
         [style '(hscroll)])
    (new canvas%
         [parent graph-panel]
         [style '(hscroll)])
    
         
         
      
    
    (send split-panel begin-container-sequence)
    (send split-panel set-percentages (list 1/3 1/3 1/3))
    (send split-panel end-container-sequence)
    ))
    