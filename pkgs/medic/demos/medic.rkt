#lang medic

;(layer layer1 
;       (in #:module "src.rkt"
;           [on-exit (void)]))
(layer layer1
       (in #:module "src.rkt"
           [(f) [on-entry (log x)]]))