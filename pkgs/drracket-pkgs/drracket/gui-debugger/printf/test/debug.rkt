#lang medic

(layer layer1 
       (in #:file "src.rkt"
           #;[each-function [on-entry (log x)]]
           [(g)
            [on-entry 
             (log x)
             (log @function-name)
             (log "x: ~a" x)
             (log "x: ~a, counter: ~a, function-name: ~a\n" x counter @function-name)
             ]]
            ))