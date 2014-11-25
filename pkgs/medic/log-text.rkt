#lang racket

(require racket/gui/base)

(provide log-text%)

(define log-text%
  (class text%
    (init-field [data null])
    (inherit insert
             begin-edit-sequence
             end-edit-sequence
             get-position
             change-style)
    (super-new)
    
    (define layer-position (make-hash))
    
    (define behavior-style (new style-delta%))
    (send behavior-style set-delta-foreground "DodgerBlue")
    
    (define/private (display-logs)
      (define current-pos (box #f))
      (begin-edit-sequence)
      (for-each
       (lambda (i)
         (define to-insert (first i))
         (define layer (second i))
         (get-position current-pos)
         (define start-pos (unbox current-pos))
         (insert to-insert)
         (insert "\n")
         (get-position current-pos)
         (define end-pos (sub1 (unbox current-pos)))
         (if (hash-has-key? layer-position layer)
             (hash-set! layer-position layer (append (hash-ref layer-position layer) 
                                                     (list (cons start-pos end-pos))))
             (hash-set! layer-position layer (list (cons start-pos end-pos))))
         (when (third i)
           (change-style behavior-style start-pos end-pos #f)))
       data)
      (end-edit-sequence))
    (display-logs)))