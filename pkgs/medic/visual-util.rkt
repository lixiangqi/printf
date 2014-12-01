#lang racket

(provide add-log
         get-log-data
         record-aggregate
         get-aggregate-data
         add-edge
         get-raw-edges
         record-timeline
         get-timeline-data)

(define log-data null)
(define snip-size 30)
(define raw-edges (make-hash))
(define timeline-table (make-hash))
(define timeline-sequence null)
(define timeline-data null)
(define aggre-table (make-hash))
(define aggre-sequence null)

(define (add-log str layer-id behavior?)
  (set! log-data (append log-data (list (list str layer-id behavior?)))))

(define (get-log-data) log-data)

(define (record-aggregate key pairs)
  (cond
    [(hash-has-key? aggre-table key)
     (hash-set! aggre-table key (append (hash-ref aggre-table key) (list pairs)))]
    [else
     (set! aggre-sequence (append aggre-sequence (list key)))
     (hash-set! aggre-table key (list pairs))]))

(define (add-edge from to edge-label from-label to-label)
  (when (and (object? from) (object? to))
    (hash-set! raw-edges (cons from to) (list edge-label from-label to-label))))

(define (get-raw-edges) raw-edges)

(define (record-timeline key label value assert?)
  (define label-str (format "~a" label))
  (cond
    [(hash-has-key? timeline-table key)
     (hash-set! timeline-table key (append (hash-ref timeline-table key) (list (list label-str value assert?))))]
    [else
     (set! timeline-sequence (append timeline-sequence (list key)))
     (hash-set! timeline-table key (list (list label-str value assert?)))]))

(define (get-timeline-data)
  (define temp null)
  (for-each
   (lambda (n)
     (set! temp (append temp (list (hash-ref timeline-table n)))))
   timeline-sequence)
  (for-each 
   (lambda (l)
     (let ([label (first (first l))]
           [values (map second l)]
           [assert? (third (first l))])
       (set! timeline-data (append timeline-data (list (list label assert? values))))))
   temp)
  (set! timeline-sequence null)
  (set! timeline-table #f)
  timeline-data)

(define (get-aggregate-data)
  (map (lambda (n) (hash-ref aggre-table n)) aggre-sequence))
  
  