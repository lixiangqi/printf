#lang medic

(layer layer1 
       (in #:file "src3.rkt"
           ; scope of multiple functions 
           [(g inc) [on-entry (log "function ~a: x = ~a" @function-name x)]]
           ; fun-pattern-expr
           [(with-start "inc") [on-entry (log "function ~a with starting inc function name" @function-name)]]
           ; each-function primitive
           [each-function [on-entry (log "function ~a entered" @function-name)]]))

