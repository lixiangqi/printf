#lang medic

(layer layer1
       (in #:module "vector.rkt"
           [(at (set! result #f)
                [#:before (= (size tempv) (add1 (size w)))])
            [on-entry 
             (log "[1]entered")
             (log (size tempv))
             (log (size w))
             ]]
           [(at (set! result #f)
                [#:before (= (element-at a i) i)])
            [on-entry 
             (log "[2]entered")]]))

(layer graph
       (in #:module "vector.rkt"
           [(prepend)
            
             [(at (set-vector-head! v temp))
              [on-exit
               
               (define tt (get-head v))
               (let loop ()
                 (when (get-next tt)
                   (let* ([next (get-next tt)])
                     (edge tt next "" "Red" (get-datum tt) (get-datum next)))
                   (set! tt (get-next tt))
                   (loop)))
               (set! tt (get-next (get-head v)))
               (let loop ()
                 (when (and tt (get-previous tt))
                   (let ([prev (get-previous tt)])
                     (edge tt prev "" #f (get-datum tt) (get-datum prev)))
                   (set! tt (get-next tt))
                   (loop)))]]]))