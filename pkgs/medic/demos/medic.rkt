#lang medic

(layer layer1 
       (in #:module "src.rkt"
           [on-entry (log 'enter)]
           #;[(g) [on-entry (void)]]))

