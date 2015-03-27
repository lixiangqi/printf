#lang medic

(layer driver 
       (in #:module "vector.rkt"
           [on-exit
            (define count 0)
            (define total 6)
            (define v (new-vector))
            (define w (new-vector))
            (for ([i (in-range 10)]) 
              (add v i)
              (add w (+ i 10)))]))

(layer last-element
       (in #:module "vector.rkt"
           [on-exit
            (with-handlers ([exn? (λ (e) (log "FAILED last-element test"))])
              (define le (last-element v))
              (cond
                [(= (first le) 9)
                 (log "  Passed last-element test . . .")
                 (set! count (add1 count))]
                [else
                 (log "  FAILED last-element test . . .")]))]))

(layer reverse
       (in #:module "vector.rkt"
           [on-exit
            (set! v (new-vector))
            (set! w (new-vector))
            (for ([i (in-range 10)]) 
              (add v i)
              (add w (+ i 10)))
            (with-handlers ([exn? (λ (e) (log "FAILED reverse test"))])
              (define r (reverse v))
              (log "r : ")
              (for ([i (in-range (size r))]) (log "~a" (element-at r i)))
              (define result #t)
              (for ([i (in-range (size r))])
                (unless (= (element-at r i) (- (size r) i 1)) (set! result #f)))
              (cond
                [result
                 (log "  Passed reverse test . . .")
                 (set! count (add1 count))]
                [else
                 (log "  FAILED reverse test . . .")]))]))

(layer append 
       (in #:module "vector.rkt"
           [on-exit
            (set! v (new-vector))
            (set! w (new-vector))
            (for ([i (in-range 10)]) 
              (add v i)
              (add w (+ i 10)))
            (with-handlers ([exn? (λ (e) (log "FAILED append test"))])
              (define a (append v w))
              (log "a : ")
              (for ([i (in-range (size a))]) (log "~a" (element-at a i)))
              (define result #t)
              (unless (= (size a) (+ (size v) (size w))) (set! result #f))
              (for ([i (in-range (size a))]) 
                (unless (= (element-at a i) i) (set! result #f)))
              (cond
                [result
                 (log "  Passed append test . . .")
                 (set! count (add1 count))]
                [else
                 (log "  FAILED append test . . .")]))]))

(layer first 
       (in #:module "vector.rkt"
           [on-exit
            (set! v (new-vector))
            (set! w (new-vector))
            (for ([i (in-range 10)]) 
              (add v i)
              (add w (+ i 10)))
            (with-handlers ([exn? (λ (e) (log "FAILED first test"))])
              (cond
                [(zero? (first v))
                 (log "  Passed first test . . .")
                 (set! count (add1 count))]
                [else
                 (log "  FAILED first test . . .")]))]))

(layer rest
       (in #:module "vector.rkt"
           [on-exit
            (set! v (new-vector))
            (set! w (new-vector))
            (for ([i (in-range 10)]) 
              (add v i)
              (add w (+ i 10)))
            (with-handlers ([exn? (λ (e) (log " FAILED rest test"))])
              (define tempv (rest v))
              (log "tempv : ")
              (for ([i (in-range (size tempv))]) (log "~a" (element-at tempv i)))
              (define result #t)
              (unless (= (size tempv) (sub1 (size v))) (set! result #f))
              (for ([i (in-range (size tempv))])
                (unless (= (element-at tempv i) (add1 i))
                  (set! result #f)))
              (cond
                [result
                 (log "  Passed rest test . . .")
                 (set! count (add1 count))]
                [else
                 (log "  FAILED rest test . . .")]))]))

(layer prepend
       (in #:module "vector.rkt"
           [on-exit
            (set! v (new-vector))
            (set! w (new-vector))
            (for ([i (in-range 10)]) 
              (add v i)
              (add w (+ i 10)))
            (with-handlers ([exn? (λ (e) (log "FAILED prepend test"))])
              (define tempv (prepend v -1))
              (log "tempv : ")
              (for ([i (in-range (size tempv))]) (log "~a" (element-at tempv i)))
              (define result #t)
              (unless (= (size tempv) (add1 (size w))) (set! result #f))
              (for ([i (in-range (size tempv))])
                (unless (= (element-at tempv i) (sub1 i))
                  (set! result #f)))
              (cond
                [result
                 (log "  Passed prepend test . . .")
                 (set! count (add1 count))]
                [else
                 (log "  FAILED prepend test . . .")])
              (log "Results: ~a / ~a" count total)
              )]))


            
            
            
