#lang racket

(provide add-log
         add-edge
         get-raw-edges
         record-timeline
         get-timeline-data)

(define snip-size 30)
(define raw-edges (make-hash))
(define timeline-table (make-hash))
(define sequence null)
(define timeline-data null)

(define (add-log str)
  (printf "add-log, str=~a\n" str)
  (void))

(define (add-edge from to edge-label from-label to-label)
  (when (and (object? from) (object? to))
    (hash-set! raw-edges (cons from to) (list edge-label from-label to-label))))

(define (get-raw-edges) raw-edges)

(define (record-timeline key label value assert?)
  (cond
    [(hash-has-key? timeline-table key)
     (hash-set! timeline-table key (append (hash-ref timeline-table key) (list (list label value assert?))))]
    [else
     (set! sequence (append sequence (list key)))
     (hash-set! timeline-table key (list (list label value assert?)))]))

(define (get-timeline-data)
  (define temp null)
  (for-each
   (lambda (n)
     (set! temp (append temp (list (hash-ref timeline-table n)))))
   sequence)
  (for-each 
   (lambda (l)
     (let ([label (first (first l))]
           [values (map second l)]
           [assert? (third (first l))])
       (set! timeline-data (append timeline-data (list (list label assert? values))))))
   temp)
  (set! sequence null)
  (set! timeline-table #f)
  timeline-data)

  