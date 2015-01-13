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
           #;[(at (inc-counter) [#:before (define x (inc 4))] [#:after (+ x 1)])
            [on-entry (printf "entering inc-counter\n")]]
           #;[(f) 
            [(at (inc-counter) [#:before (define x (inc 4))] [#:after (+ x 1)])
            [on-entry (printf "entering inc-counter\n")]]]
           [each-function [on-entry (printf "aha\n")]]
           [(ff) [on-entry (printf "f entered.\n")]
                 [on-exit (printf "f exited.\n")]
                 ]))


;(search-pos s #'(inc-counter) (list #'(define x (inc 4))) null)
;
;(search-pos s #'(inc-counter) (list #'(define x (inc 4))) (list #'(+ x 1)))
;
;(search-pos s #'(inc-counter) (list #'(define x (inc 4))) (list #'(+ x 2)))
;
;(search-pos s #'(inc-counter) (list #'(define x (inc 4))) (list #'(+ x 1) #'(+ y 3)))
;
;(search-pos s #'(inc-counter2) (list #'(define x (inc 4))) (list #'(+ x 1)))
;
;(search-pos s #'(inc-counter) null null)
;
;(search-pos s #'(inc-counter) (list #'(define x 3)) null)
;
;(search-pos s #'(inc-counter) (list #'(define x (inc 4)) #'(+ x 1)) null)
;
;(search-pos s #'(inc-counter) null (list #'(+ x 1)))