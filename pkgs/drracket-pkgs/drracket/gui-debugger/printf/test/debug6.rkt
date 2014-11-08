#lang medic

(layer layer1
       (in #:file "src6.rkt"
           (with-action fact "function fact takes @x and returns @,ret")
           [on-exit
            (define test%
              (class object%
                (super-new)))
            (define a (new test%))
            (define b (new test%))
            (define c (new test%))
            (edge a b "ab")
            (edge b c "bc")]
           [(fact) [on-entry 
                    (printf "[debug]layer1 entered.\n")]]))