#lang medic

(layer layer1 
       (in #:file "src.rkt"
           (with-behavior g "function g takes @,x and @,x returns @ret")
           [on-exit (log (g 50))]))