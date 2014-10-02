#lang racket

(require racket/gui
         syntax/modread)

(provide eval/annotations)

; (struct at-insert (scope target before after loc insert-exprs) #:transparent)

; filename: complete-path-string
(define (build-input-port filename at-table)
  (printf "build-input-port ... \n")
  (define text (make-object text%))
  (send text insert-file filename)
  (printf "get-text:~v\n" (send text get-text))
  (printf "search-res=~v\n" (send text find-string "(define y 3)" 'forward 0))
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
      (values p filename))))

(define (eval/annotations initial-module annotate-module? annotator insert-tables at-tables)
  (parameterize
      ([current-load/use-compiled
        (let ([ocload/use-compiled (current-load/use-compiled)])
          (lambda (fn m)
            (cond [(annotate-module? fn m)
                   (let* ([fn-str (path->string fn)]
                          [insert-table (hash-ref insert-tables fn-str #f)]
                          [at-table (hash-ref at-tables fn-str #f)])
                     (load-module/annotate annotator fn m insert-table at-table))]
                  [else
                   (ocload/use-compiled fn m)])))]
       [current-namespace (make-base-namespace)])
    (eval #`(require #,initial-module))
    
;     (define fn "/home/xiangqi/printf/racket/pkgs/drracket-pkgs/drracket/gui-debugger/printf/test/src3.rkt")
;     (build-input-port fn (hash-ref at-tables fn #f))
    ))

; fn: complete file path
(define (load-module/annotate annotator fn m insert-table at-table)
  (let-values ([(base _ __) (split-path fn)]
                 [(in-port src) (build-input-port fn at-table)])
    (dynamic-wind
     (lambda () (void))
     
     (lambda ()
       (parameterize ([read-accept-compiled #f]
                      [current-load-relative-directory base])
         (unless m (raise 'module-name-not-passed-to-load-module/annotate))
         (with-module-reading-parameterization
          (lambda ()
            (let* ([first (parameterize ([current-namespace (make-base-namespace)])
                            (expand (read-syntax src in-port)))]
                   [module-ized-exp (annotator (check-module-form first m fn) insert-table)]
                   [second (read in-port)])
              (unless (eof-object? second)
                (raise-syntax-error
                 'load-module/annotate
                 (format "expected only a `module' declaration for `~s', but found an extra expression" m)
                 second))
              (eval-syntax module-ized-exp))))))
     
     (lambda () (close-input-port in-port)))))
