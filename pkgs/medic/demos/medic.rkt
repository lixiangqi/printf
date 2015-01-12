#lang medic

;(layer layer1 
;       (in #:module "src.rkt"
;           [on-exit (void)]))

#;(layer layer1
       (export x)
       (def x #:src (void))
       (in #:module "src.rkt"
           [(f) 
            [(at (+ x 1)) [on-entry 'aha]]]
           [(f)
            [(at (+ x 1)) [on-exit 'heih]]]
           [(at (+ x 3)) [on-entry 'hello]]))

(layer layer1
       (in #:module "src.rkt"
           [on-exit (void)]))