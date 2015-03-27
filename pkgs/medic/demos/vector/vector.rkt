#lang racket

(provide (all-defined-out))

(struct vector ([size #:mutable] [head #:mutable] [last #:mutable]) #:transparent)
(struct vector-element ([next #:mutable] [previous #:mutable] datum) #:transparent)

(define (new-vector) (vector 0 #f #f))
(define (new-vector-element datum) (vector-element #f #f datum))
(define (new-vector-with-datum datum)
  (define v (vector 1 (new-vector-element datum) #f))
  (set-vector-last! v (vector-head v))
  v)

(define (get-head v) (vector-head v))
(define (get-last v) (vector-last v))
(define (get-next e) (vector-element-next e))
(define (get-previous e) (vector-element-previous e))
(define (get-datum e) (vector-element-datum e))

(define (copy-vector v1 v2)
  (for ([i (in-range (size v2))])
    (let ([temp (element-at v2 i)])
      (add v1 temp)))
  v1)

(define (last-element cur-v)
  (define l (vector-head cur-v))
  (let loop ()
    (when (vector-element-next l)
      (set! l (vector-element-next l))
      (loop)))
  (define v (new-vector-with-datum (vector-element-datum l)))
  v)

(define (first v)
  (vector-element-datum (vector-head v)))

(define (rest v)
  (define nv v)
  (define ve (vector-head nv))
  (set! ve (vector-element-next ve))
  (set-vector-head! nv ve)
  nv)

(define (element-at v index)
  (when (or (> index (sub1 (vector-size v))) (< index 0))
    (error 'element-at-not-a-valid-index))
  (define temp (vector-head v))
  (let loop ()
    (unless (zero? index)
      (set! temp (vector-element-next temp))
      (set! index (sub1 index))
      (loop)))
  (vector-element-datum temp))

(define (size v) (vector-size v))

(define (reverse cur-v)
  (when (zero? (vector-size cur-v)) (error 'reverse-empty-vector))
  (define nv (copy-vector (new-vector) cur-v))
  (define temp (vector-head nv))
  (set-vector-head! nv (vector-last nv))
  (set-vector-last! nv temp)
  nv
  )
    
(define (append cur-v v)
  (define v1 cur-v)
  (define temp1 (vector-head v))
  (let loop ()
    (when (vector-element-next temp1)
      (let ([tem (vector-element-datum temp1)])
        (add v1 tem)
        (set! temp1 (vector-element-next temp1)))
      (loop)))
  (define temp (vector-element-datum temp1))
  (add v1 temp)
  cur-v)
  
(define (prepend cur-v datum)
  (define v cur-v)
  (define temp (new-vector-element datum))
  (define temp1 (vector-head v))
  (set-vector-element-next! temp temp1)
  (set-vector-element-previous! temp1 temp)
  (set-vector-head! v temp)
  v)
  

(define (add v datum)
  (cond
    [(zero? (vector-size v))
     (set-vector-head! v (new-vector-element datum))
     (set-vector-last! v (vector-head v))
     (set-vector-size! v 1)]
    [else
     (define temp (new-vector-element datum))
     (define v-last (vector-last v))
     (set-vector-element-previous! temp v-last)
     (set-vector-element-next! v-last temp)
     (set-vector-last! v temp)
     (set-vector-size! v (add1 (vector-size v)))])
  #t)

(define (add-at v index datum)
  (when (or (< index 0) (> index (vector-size v)))
    (error 'add-at-not-a-valid-index))
  (cond
    [(= index (vector-size v))
     (add v datum)]
    [(zero? index)
     (define temp (new-vector-element datum))
     (set-vector-element-next! temp (vector-head v))
     (set-vector-element-previous! (vector-head v) temp)
     (set-vector-head! v temp)
     (set-vector-size! v (add1 (vector-size v)))]
    [else
     (define temp (new-vector-element datum))
     (define ip (vector-head v))
     (for ([i (in-range index)]) (set! ip (vector-element-next ip)))
     (set-vector-element-next! temp ip)
     (set-vector-element-previous! temp (vector-element-previous ip))
     (set-vector-element-next! (vector-element-previous ip) temp)
     (set-vector-element-previous! ip temp)
     (set-vector-size! v (add1 (vector-size v)))]))

(define (remove v index)
  (when (or (< index 0) (> index (sub1 (vector-size v))))
    (error 'remove-not-a-valid-index))
  (cond
    [(zero? index)
     (define result (vector-element-datum (vector-head v)))
     (set-vector-head! v (vector-element-next (vector-head v)))
     (if (vector-head v)
         (set-vector-element-previous! (vector-head v) #f)
         (set-vector-last! v #f))
     (set-vector-size! v (sub1 (vector-size v)))
     result]
    [(= index (sub1 (vector-size v)))
     (define result (vector-element-datum (vector-last v)))
     (set-vector-last! v (vector-element-previous (vector-last v)))
     (set-vector-element-next! (vector-last v) #f)
     (set-vector-size! v (sub1 (vector-size)))
     result]
    [else
     (define temp (vector-head v))
     (for ([i (in-range index)]) (set! temp (vector-element-next temp)))
     (define result (vector-element-datum temp))
     (set-vector-element-next! (vector-element-previous temp) (vector-element-next temp))
     (set-vector-element-previous! (vector-element-next temp) (vector-element-previous temp))
     (set-vector-size! v (sub1 (vector-size v)))
     result]))