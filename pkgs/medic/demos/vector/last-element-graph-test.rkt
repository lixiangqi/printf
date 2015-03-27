#lang medic

(layer le-graph
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
            (with-handlers ([exn? (λ (e) (log "FAILED last-element test"))])
              (define le (last-element v))
              (define head (get-head le))
              (define s (string-append "last-element: " (format "~a" (get-datum head))))
              ; testing whether return a new vector element
              (node head s "red")
              (render-graph v))]))


