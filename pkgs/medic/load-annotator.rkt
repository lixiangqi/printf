#lang racket

(require racket/gui
         syntax/modread
         "medic-structs.rkt"
         "insert.rkt")

(provide eval/annotations)

; filename: complete-path-string
(define (build-input-port filename at-table)
  (define text (make-object text%))
  (send text insert-file filename)
  (define new-at-table
    (map (lambda (entry)
           (let* ([positions (send text find-string-all (at-insert-target entry) 'forward  0)]
                  [filtered
                   (filter (lambda (p)
                             (and (if (equal? (at-insert-before entry) '())
                                      #t
                                      (andmap (lambda (e) (send text find-string e 'backward p)) (at-insert-before entry)))
                                  (if (equal? (at-insert-after entry) '())
                                      #t
                                      (andmap (lambda (e) (send text find-string e 'forward p)) (at-insert-after entry)))))
                           positions)]
                  [possible-posns (map add1 filtered)]) ; need to increment one to match the position returned by syntax-position later
             (finer-at-insert (at-insert-scope entry) (at-insert-target entry) possible-posns (at-insert-loc entry) (at-insert-exprs entry))))
         at-table))
  ; filter out empty posns in finer-at-insert structure
  (set! new-at-table (filter (lambda (a) (not (null? (finer-at-insert-posns a)))) new-at-table))
  (let ([p (open-input-file filename)])
    (port-count-lines! p)
    (let ([p (cond [(regexp-match-peek "^WXME01[0-9][0-9] ## " p)
                    (let ([t (make-object text%)])
                      (send t insert-file p 'standard)
                      (close-input-port p)
                      (open-input-text-editor t))]
                   [else p])])
      (port-count-lines! p)
      (let loop ()
        (when (regexp-match-peek "^#!" p)
          (let lloop ([prev #f])
            (let ([c (read-char-or-special p)])
              (if (or (eof-object? c)
                      (eq? c #\return)
                      (eq? c #\newline))
                  (when (eq? prev #\\)
                    (loop))
                  (lloop c))))))
      (values p filename new-at-table))))

(define (eval/annotations initial-module annotate-module? annotator insert-tables at-tables)
  (parameterize
      ([current-load/use-compiled
        (let ([ocload/use-compiled (current-load/use-compiled)])
          (lambda (fn m)
            (cond [(annotate-module? fn m)
                   (let* ([fn-str (path->string fn)]
                          [insert-table (hash-ref insert-tables fn-str '())]
                          [at-table (hash-ref at-tables fn-str '())])
                     (load-module/annotate annotator fn m insert-table at-table))]
                  [else
                   (ocload/use-compiled fn m)])))]
       [current-namespace (make-base-namespace)])
    (eval #`(require #,initial-module))))

; fn: complete-path-string
(define (load-module/annotate annotator fn m insert-table at-table)
  (let-values ([(base _ __) (split-path fn)]
               [(in-port src new-at-table) (build-input-port fn at-table)])
    (dynamic-wind
     (lambda () (void))
     
     (lambda ()
       (parameterize ([read-accept-compiled #f]
                      [current-load-relative-directory base])
         (unless m (raise 'module-name-not-passed-to-load-module/annotate))
         (with-module-reading-parameterization
          (lambda ()
            (let* ([stx (parameterize ([current-namespace (make-base-namespace)])
                            (read-syntax src in-port))]
                   [inserted (expand (insert-stx (check-module-form (expand stx) m fn) insert-table new-at-table))]
                   [module-ized-exp (annotator (check-module-form inserted m fn) insert-table new-at-table)]
                   [second (read in-port)])
              (unless (eof-object? second)
                (raise-syntax-error
                 'load-module/annotate
                 (format "expected only a `module' declaration for `~s', but found an extra expression" m)
                 second))
              (eval-syntax module-ized-exp))))))
     
     (lambda () (close-input-port in-port)))))
