#lang medic

(layer layer1
       (in #:module "src.rkt"
           [(at (printf "Working hard... ~a~n" i)) 
            [on-entry 
             (timeline i)]]))