#lang medic

(layer prepend
       (in #:module "vector.rkt"
           [on-exit
            (define (render-graph v)
              (define tt (get-head v))
              (node tt (string-append "head: " (format "~a" (get-datum tt))) "black")
              (node (get-last v) 
                    (string-append "last: " (format "~a" (get-datum (get-last v)))) "pink")
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
                  (loop))))
            (define v (new-vector))
            (define w (new-vector))
            (for ([i (in-range 10)]) 
              (add v i)
              (add w (+ i 10)))
            (with-handlers ([exn? (λ (e) (log "FAILED prepend test"))])
              (define tempv (prepend v -1))
              (render-graph tempv)
              (log (size tempv))
              (render-graph v)
              (log (size v))
              )]))


