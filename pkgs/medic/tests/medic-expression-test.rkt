#lang medic


; test match-expr
(layer layer1 
       (in #:file "factorial.rkt"
           ; insert-expr case
           [on-entry (define x 1)
                     (define y 1)]
           [on-exit (define z 1)]
           [(at (define x 1)) [on-entry (define q 1)]
                              [on-exit (define w 1)]]
           [(at (with-start |(define x|)) [on-entry (define p 1)]]
           [(at (define x 1) [#:before (define e 1)]) [on-entry (define m 1)]]
           [(at (define x 1) [#:after (define e 1)]) [on-entry (define n 1)]]
           ; each-function case
           [each-function (on-entry (define o 2))]
           [each-function [(at (define x 1)) [on-entry (define y 1)]]]
           ; function-patter case
           [(with-start |fact|) [on-entry (define u 1)]]
           ; function case
           [(fact g) [on-entry (define i 1)]]
           [(g) [on-exit (define e 2)]]
           [(g) [(at (define p 2)) [on-entry (define b 3)]]]))

; test (ref debug-id)
(layer layer2
      (def s #:debug [on-entry (define x 1)]
                     [on-exit (define y 1)
                              (define z 1)])
      (in #:file "factorial.rkt"
          (ref s)))

; test reference to undefined debugging identifier
;(layer layer3
;       (in #:file "factorial.rkt"
;          (ref s)))

; test src-id
(layer layer4
       (def s1 #:src (define x 1))
       (def s2 #:src (define y 2)
                     (ref s1)
                     (define z 3))
       (def s3 #:src (ref s2)
                     (define a 4))
       (in #:file "factorial.rkt"
           [each-function [on-entry (ref s3)]]))
           
; multiple file in one single layer
(layer layer5
       (def s3 #:src (define x 1))
       (in #:file "factorial.rkt"
           [each-function [on-entry (ref s3)]])
       (in #:file "g.rkt"
           [each-function [on-entry (ref s3)]]))

; multiple layers export and import
(layer layer6
       (export s3)
       (def s1 #:src (define x 1))
       (def s2 #:src (define y 2)
                     (ref s1)
                     (define z 3))
       (def s3 #:src (ref s2)
                     (define b 4)))

(layer layer7
       (export s1 d1)
       (import layer6)
       (def s1 #:src (define m 1))
       (def d1 #:debug [each-function [on-entry (ref s3)]]) 
       (in #:file "g.rkt"
           (ref d1)))

(layer layer8
       (import layer6 layer7)
       (def s2 #:src (ref s3))
       (in #:file "g.rkt"
           [on-exit (ref s1)]
           (ref d1)))

       
; test layer syntax property
(layer layer9
       (export s3 s4)
       (def s1 #:src (define x 1)
                     (printf "x=~v\n" x))
       (def s2 #:src (define y 2)
                     (ref s1)
                     (define z 3))
       (def s3 #:src (ref s2)
                     (define a 4))
       (def s4 #:debug [(fact) [on-exit (printf "y=~a\n" y)]])
       (in #:file "factorial.rkt"
           [each-function [on-entry (ref s3)]]
           (ref s4)))

(layer layer10
       (import layer9)
       (in #:file "factorial.rkt"
           [(g) [on-entry (ref s3)]]
           (ref s4)))

; test layer syntax property
(layer layer11
       (export s1 d1)
       (def s1 #:src (define x 1)
                     (printf "x=~v\n" x))
       (def d1 #:debug [(fact) [on-exit (printf "y=~v\n" y)]])
       (in #:file "factorial.rkt"
           (ref d1)))

(layer layer12
       (import layer11)
       (in #:file "factorial.rkt"
           [(g) [on-entry (ref s1)]]
           (ref d1)))