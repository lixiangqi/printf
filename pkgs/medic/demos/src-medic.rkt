#lang medic

(layer layer1
       (in #:module "src.rkt"
           [each-function [on-exit (log "~a exited, a=~a" @function-name a)]]))