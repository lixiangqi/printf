#lang racket

(define node%
  (class object%
    (super-new)
    (init-field [datum 0])
    (field [next #f]
           [previous #f])
    
    (define/public (get-next) next)
    (define/public (get-datum) datum)
    ))

(define doubly-linked-list%
  (class object%
    (field [head #f]
           [last #f])
    (super-new)
    (define size 0)
    
    (define/public (init d)
      (set! head (new node% [datum d]))
      (set! last head)
      (set! size 1))
    
    (define/public (element-at i)
      (when (or (> i (sub1 size)) (< i 0))
        (error 'element-at-invalid-argument))
      (define temp head)
      (let loop ()
        (when (not (zero? i))
          (set! temp (get-field next temp))
          (set! i (sub1 i))
          (loop)))
      (get-field datum temp))
    
    (define/public (get-size) size)
    
    (define/public (add d)
      (cond
        [(zero? size) (init d)]
        [else
         (define temp (new node% [datum d]))
         (set-field! previous temp last)
         (set-field! next last temp)
         (set! last temp)
         (set! size (add1 size))]))
    
    (define/public (add-at i d)
      (when (or (< i 0) (> i size))
        (error 'add-invalid-arguments))
      (if (= i size)
          (add d)
          (cond
            [(zero? i)
             (define temp (new node% [datum d]))
             (set-field! next temp head)
             (set-field! previous head temp)
             (set! head temp)
             (set! size (add1 size))]
            [else
             (define temp (new node% [datum d]))
             (define p head)
             (for ([j (in-range i)])
               (set! p (get-field next p)))
             (set-field! next temp p)
             (define p-prev (get-field previous p))
             (set-field! previous temp p-prev)
             (set-field! next p-prev temp)
             (set-field! previous p temp)
             (set! size (add1 size))])))
    
    (define/public (remove i)
      (when (or (< i 0) (> i (sub1 size)))
        (error 'remove-invalid-argument))
      (cond
        [(zero? i)
         (define res (get-field datum head))
         (set! head (get-field next head))
         (if head
             (set-field! previous head #f)
             (set! last #f))
         (set! size (sub1 size))
         res]
        [else
         (cond
           [(= i (sub1 size))
            (define res (get-field datum last))
            (set! last (get-field previous last))
            (set-field! next last #f)
            (set! size (sub1 size))
            res]
           [else
            (define temp head)
            (for ([j (in-range i)]) (set! temp (get-field next temp)))
            (define res (get-field datum temp))
            (define temp-prev (get-field previous temp))
            (define temp-next (get-field next temp))
            (set-field! next temp-prev temp-next)
            ;(set-field! previous temp-next temp-prev)
            (set! size (sub1 size))
            res])]))))

;(define dlist (new doubly-linked-list%))
;(for ([i (reverse (build-list 10 values))]) (send dlist add-at 0 i))
;  
;(for ([i (in-range (send dlist get-size))])
;  (printf "i=~v, datum=~v\n" i (send dlist element-at i)))
;
;
;(for ([i (in-range 5)]) (send dlist remove 3))
;(for ([i (in-range (send dlist get-size))])
;  (printf "after: i=~v, datum=~v\n" i (send dlist element-at i)))  