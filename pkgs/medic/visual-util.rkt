#lang racket

(provide add-log
         add-edge
         record-aggregate
         record-timeline
         record-changed
         get-log-data
         get-raw-edges
         get-aggregate-data
         get-timeline-data
         get-changed-data)

(define log-data null)
(define snip-size 30)
(define raw-edges (make-hash))
(define timeline-table (make-hash))
(define timeline-sequence null)
(define aggre-table (make-hash))
(define aggre-sequence null)
(define identifiers null)
(define changed-table (make-hash))

(define (add-log str layer-id behavior?)
  (set! log-data (append log-data (list (list str layer-id behavior?)))))

(define (get-log-data) log-data)

(define (record-aggregate key labels vals)
  (define pairs (map (lambda (l v) (cons l v)) labels vals))
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

(define (record-changed id-stx label val)
  (define label-str (format "~a" label))
  (define len (length identifiers))
  (let loop ([i 0])
    (if (< i len)
        (if (free-identifier=? (list-ref identifiers i) id-stx)
            (hash-set! changed-table i (append (hash-ref changed-table i) (list (list label-str val))))
            (loop (add1 i)))
        (begin
          (set! identifiers (append identifiers (list id-stx)))
          (hash-set! changed-table i (list (list label-str val)))))))

(define (record-timeline key label value boolean?)
  (cond
    [(hash-has-key? timeline-table key)
     (hash-set! timeline-table key (append (hash-ref timeline-table key) (list (list label value boolean?))))]
    [else
     (set! timeline-sequence (append timeline-sequence (list key)))
     (hash-set! timeline-table key (list (list label value boolean?)))]))

(define (get-timeline-data)
  (define data (for/list ([i timeline-sequence])
                 (let* ([val (hash-ref timeline-table i)]
                        [label (first (first val))]
                        [values (map second val)]
                        [boolean? (third (first val))])
                   (list label boolean? values))))
  (set! timeline-sequence null)
  (set! timeline-table #f)
  data)

(define (get-aggregate-data)
  (define data (map (lambda (n) (hash-ref aggre-table n)) aggre-sequence))
  (set! aggre-table #f)
  (set! aggre-sequence null)
  data)

;; data is like
;; ("x" #t (7 5)) 
;; ("x" #t (-1 a)
;; should process it use equal? to return only boolean values
;; make inspector
(define (get-changed-data)
  (define data (for/list ([i (in-range (length identifiers))])
                 (let* ([val (hash-ref changed-table i)]
                        [label (first (first val))]
                        [values (map second val)])
                   (list label #t values))))
  (set! identifiers null)
  (set! changed-table #f)
  data)