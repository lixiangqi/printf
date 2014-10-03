#lang racket

(require racket/gui
         syntax/modread
         "medic-annotator.rkt"
         "medic-interpreter.rkt")

(define mac-path "/Users/Debbie/Documents/racket_code/Sep/example/factorial.rkt")
(define unix-path "/home/xiangqi/work/Dec/debug/factorial.rkt")

; TODO: the string supplied to #:file can be a relative path or complete path
(define debug-stx #`(medic
                     (layer layer1 
                            (in #:file #,unix-path
                                [(fact) [on-entry (printf "x = ~a\n" x)
                                                  ]]))))

(define insert-table (hash-ref (interpret debug-stx) unix-path))

(define (build-input-port filename)
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


(define (eval/annotations initial-module annotate-module? annotator)
    (parameterize
      ([current-load/use-compiled
        (let ([ocload/use-compiled (current-load/use-compiled)])
          (lambda (fn m)
            (cond [(annotate-module? fn m)
                   (load-module/annotate annotator fn m)]
                  [else
                   (ocload/use-compiled fn m)])))])
      (eval #`(require (file "/home/xiangqi/work/Dec/debug/factorial.rkt")))))  ; updated later!!!!

(define (load-module/annotate fn m)
  (let-values ([(base _ __) (split-path fn)]
               [(in-port src) (build-input-port fn)])
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
                   [module-ized-exp (annotate-stx (check-module-form first m fn) insert-table)]
                   [second (read in-port)])
              (unless (eof-object? second)
                (raise-syntax-error
                 'load-module/annotate
                 (format "expected only a `module' declaration for `~s', but found an extra expression" m)
                 second))
              (eval-syntax module-ized-exp))))))
     
     (lambda () (close-input-port in-port)))))


(eval/annotations #t (lambda (fn m)
                       (printf "fn=~v\n" fn)
                       #f) annotate-stx)

;(load-module/annotate unix-path 'factorial)
;
;(printf "--------------run-----------\n")
;(dynamic-require ''factorial #f)

