#lang medic

(layer layer1
       (in #:file "src.rkt"
          
           
           [on-exit 
            (define test%
              (class object%
                (super-new)))
            (define a (new test%))
            (define b (new test%))
            (define c (new test%))
            (define d (new test%))
            (define e (new test%))
            (define f (new test%))
            (edge a b "ab" "a" "b")
            (edge b a #f "b" "a" "red")
            
            (edge b c #f "b" "c")
            (edge c b #f "c" "b" "red")
            
            (edge d b "dttttb" "d" "b")
            (edge b d "bd" "b" "d" "red")
            
            (edge e b #f "e" "b")
            (edge b e "be" "b" "e" "red")
            
            (edge b f "bf" "b" "f")
            ]))