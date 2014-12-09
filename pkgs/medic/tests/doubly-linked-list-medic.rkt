#lang medic

(layer layer1
       (in #:file "doubly-linked-list.rkt"
          [on-exit
           (define dlist (new doubly-linked-list%))
           (for ([i (reverse (build-list 10 values))]) (send dlist add-at 0 i))
            
;           (send dlist remove 3)
;           (send dlist remove 3)
;           (send dlist remove 3)
;           (send dlist remove 3)
;           (send dlist remove 3)
            
           (for/fold ([temp (get-field head dlist)]) 
                     ([i (in-range (sub1 (send dlist get-size)))])
             (define next (get-field next temp))
             (edge temp next "" (get-field datum temp) (get-field datum next) "Red")
             next)
            
           (for/fold ([temp (get-field next (get-field head dlist))])
                     ([i (in-range (sub1 (send dlist get-size)))])
             (define prev (get-field previous temp))
             (edge temp prev "" (get-field datum temp) (get-field datum prev))
             (get-field next temp))]))
