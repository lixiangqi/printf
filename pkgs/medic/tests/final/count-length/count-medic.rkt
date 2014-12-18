#lang medic

(layer layer1
       (in #:file "count.rkt"
           [(count-length) [on-entry (timeline count)
                                     (timeline v)
                                     (timeline (null? v))]]))