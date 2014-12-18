#lang medic
 
(layer left-path
       (in #:file "find-path.rkt"
           [(at (with-start "(if left-p")) [on-entry (log "left branch: ~a, ~a" (cadr t) left-p)]]))
 
(layer right-path
       (in #:file "find-path.rkt"
           [(at (with-start "(if right-p")) [on-entry (log "right branch: ~a, ~a" (caddr t) right-p)]]))
