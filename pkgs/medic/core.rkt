#lang racket

(require "load-annotator.rkt"
         "medic-annotator.rkt")

(provide medic
         debug)

(define medic-insert-table #f)
(define medic-at-table #f)

; fn can be relative-path or complete path
; e.g. (medic "/home/xiangqi/printf/racket/pkgs/drracket-pkgs/drracket/gui-debugger/printf/test/debug1.rkt"
;                 "debug2.rkt")

(define (medic fn . fns)
  (let* ([files (map (lambda (f) (if (complete-path? f) `(file ,f) f)) (append (list fn) fns))]
         [tables (map (lambda (l) (dynamic-require l 'medic-table)) files)]
         [insert-tables (map car tables)]
         [insert-table (hash-copy (first insert-tables))]
         [at-tables (map cdr tables)]
         [at-table (hash-copy (first at-tables))])
    (for ([t (rest insert-tables)])
      (for ([fn-key (hash-keys t)])
        (cond
          [(hash-has-key? insert-table fn-key) 
           (let ([scope-table1 (hash-ref insert-table fn-key)]
                 [scope-table2 (hash-ref t fn-key)])
             (for ([scope-key (hash-keys scope-table2)])
               (let ([val2 (hash-ref scope-table2 scope-key)])
               (if (hash-has-key? scope-table1 scope-key)
                   (hash-set! scope-table1 scope-key (append (hash-ref scope-table1 scope-key) val2))
                   (hash-set! scope-table1 scope-key val2)))))]
          [else
           (hash-set! insert-table fn-key (hash-ref t fn-key))])))
    (for ([t (rest at-tables)])
      (for ([fn-key (hash-keys t)])
        (let ([val (hash-ref t fn-key)])
          (if (hash-has-key? at-table fn-key)
            (hash-set! at-table fn-key (append (hash-ref at-table fn-key) val))
            (hash-set! at-table fn-key val)))))
    (set! medic-insert-table insert-table)
    (set! medic-at-table at-table)))

; fn: string of complete path or relative path
(define (debug fn)
  (let ([mod (if (complete-path? fn) `(file ,fn) fn)]
        [annotate-module? (lambda (fn m)
                            (let ([fn-str (path->string fn)])
                              (or (and medic-insert-table (hash-has-key? medic-insert-table fn-str))
                                  (and medic-at-table (hash-has-key? medic-at-table fn-str)))))])
    (eval/annotations mod annotate-module? annotate-stx medic-insert-table medic-at-table)))