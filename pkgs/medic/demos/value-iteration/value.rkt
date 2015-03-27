#lang racket

(define v (make-hash (list (cons 'a 0) (cons 'b 0) (cons 'c 0) (cons 'd 0) (cons 'e 0)
                           (cons 'f 0) (cons 'g 0) (cons 'h 0) (cons 'i 0) (cons 'j 0)
                           (cons 'k 0) (cons 'l 0) (cons 'm 0) (cons 'n 0))))

(define q (make-hash (list (cons 'a (cons 0 0)) (cons 'b (cons 0 0)) (cons 'c (cons 0 0)) 
                           (cons 'd (cons 0 0)) (cons 'e (cons 0 0))
                           (cons 'f 0) (cons 'g 0) (cons 'h 0) (cons 'i (cons 0 0)) 
                           (cons 'j (cons 0 0)) (cons 'k (cons 0 0)) (cons 'l (cons 0 0))
                           (cons 'm (cons 0 0)) (cons 'n (cons 0 0)))))

(define temp1 (hash-copy v))
(define temp2 (hash-copy q))

(for ([gamma (list 1.0 0.5 0.3 0.1)])
  (set! v (hash-copy temp1))
  (set! q (hash-copy temp2))
;  (printf "\ngamma=~a\n" gamma)
;  (printf "v=~a\n" v)
  (let loop ([i 1])
    (when (< i 100)
      (hash-set! q 'a (cons (* gamma (hash-ref v 'b)) (* gamma (hash-ref v 'n))))
      (hash-set! q 'b (cons (* gamma (hash-ref v 'a)) (* gamma (hash-ref v 'c))))
      (hash-set! q 'c (cons (* gamma (hash-ref v 'b)) (sub1 (* gamma (hash-ref v 'd)))))
      (hash-set! q 'd (cons (* gamma (hash-ref v 'c)) (sub1 (* gamma (hash-ref v 'e)))))
      (hash-set! q 'e (cons (* gamma (hash-ref v 'd)) (+ 2 (* gamma (hash-ref v 'f)))))
      (hash-set! q 'f (+ 2 (* gamma (hash-ref v 'g))))
      (hash-set! q 'g (* gamma (hash-ref v 'h)))
      (hash-set! q 'h (* gamma (hash-ref v 'i)))
      (hash-set! q 'i (cons (* gamma (hash-ref v 'h)) (* gamma (hash-ref v 'j))))
      (hash-set! q 'j (cons (* gamma (hash-ref v 'k)) (* gamma (hash-ref v 'i))))
      (hash-set! q 'k (cons (* gamma (hash-ref v 'l)) (* gamma (hash-ref v 'j))))
      (hash-set! q 'l (cons (* gamma (hash-ref v 'm)) (* gamma (hash-ref v 'k))))
      (hash-set! q 'm (cons (* gamma (hash-ref v 'n)) (* gamma (hash-ref v 'l))))
      (hash-set! q 'n (cons (* gamma (hash-ref v 'a)) (* gamma (hash-ref v 'm))))
      
      (for ([k (hash-keys v)])
        (cond
          [(or (equal? k 'f) (equal? k 'g) (equal? k 'h))
           (hash-set! v k (hash-ref q k))]
          [else
           (hash-set! v k (max (car (hash-ref q k)) (cdr (hash-ref q k))))]))
      
      ;(printf "i: ~a\n" i)
      ;(printf "v[a]: ~a\n" (hash-ref v 'a))
      ;(printf "v[e]: ~a\n" (hash-ref v 'e))
      (loop (add1 i)))))