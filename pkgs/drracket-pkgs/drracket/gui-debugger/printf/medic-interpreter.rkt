#lang racket

; how to deal with if, cond, conditional? or other block structure?


; module to be list table
; because there maybe too many function, make a hashtable
; make all the tables to be local and to be the return value of interpret.

(provide interpret)

(define (interpret stx)
  
  (define src-env (make-hasheq))
  (define debug-env (make-hasheq))
  (define insert-table (make-hasheq))
  
  (define (interpret-layer-form stx)
    (syntax-case stx (layer)
      [(layer layer-id layer-expr ...) (identifier? #'layer-id)
                                       (for-each (lambda (expr) (interpret-layer-expr (syntax-property expr 'layer (syntax->datum #'layer-id)))) 
                                                 (syntax->list #'(layer-expr ...)))]
      [else
       (error 'invalid-debugging-expression "expr = ~a\n" (syntax->datum stx))]))
  
  (define (interpret-layer-expr stx)
    (syntax-case stx (export import)
      [(export id ...) 
       (void)]
      [(import id ...)
       (void)]
      [else (interpret-expr stx)]))
  
  (define (interpret-expr stx)
    (syntax-case stx (def in)
      [(def src-id #:src src-expr ...) (identifier? #'src-id)
                                       ;(for-each (lambda (e) (interpret-src-expr e)) (syntax->list #'(src-expr ...)))
                                       ; comment out for lazy evaluation, treat def like lambda abstraction.
                                       (hash-set! src-env (syntax->datum #'src-id) #'(src-expr ...))]
      [(def debug-id #:debug expr ...) (identifier? #'debug-id)
                                       (hash-set! debug-env (syntax->datum #'debug-id) #'(expr ...))]
      [(in #:file id expr ...)
       (let ([filename (syntax->datum #'id)])
         (unless (hash-has-key? insert-table filename)
           (hash-set! insert-table filename (make-hasheq)))
         (for-each (lambda (e) (interpret-match-expr (syntax-property (syntax-property e 'filename (syntax->datum #'id))
                                                                      'layer
                                                                      (syntax-property stx 'layer))
                                                     (syntax->datum #'id))) 
                   (syntax->list #'(expr ...))))]
      [else
       (error 'invalid-debugging-expression "expr = ~a\n" (syntax->datum stx))]))
  
  ; double-check
  (define (interpret-match-expr stx filename)
    (syntax-case stx (on-entry on-exit within ref at each-function with-start each-expression) ; to-update
      [[on-entry src-expr ...]
       (interpret-insert-expr stx filename 'entry)]
      [[on-exit src-expr ...]
       (interpret-insert-expr stx filename 'exit)]
      [[(at each-expression) border-expr ...]
       (interpret-at-expr stx)]
      [[(at location-expr #:before expr ...) border-expr ...]
       (interpret-at-expr stx)]
      [[(at location-expr #:after expr ...) border-expr ...]
       (interpret-at-expr stx)]
      [[(at location-expr) border-expr ...]
       (interpret-at-expr stx)]
      [(ref debug-id)
       (hash-ref debug-env (syntax->datum #'debug-id) 'debug-env-lookup-failure)]
      [[each-function to-insert ...]
       (for-each (lambda (e) (interpret-insert-expr e filename 'each-function)) (syntax->list #'(to-insert ...)))]
      [(with-start |part-of-fun-name| to-insert ...)
       (for-each (lambda (e) (interpret-insert-expr e filename 'with)) (syntax->list #'(to-insert ...)))]
      [[(f) to-insert ...] ; delete this case later after, the same as the below (f ...)
       (for-each (lambda (e) (interpret-insert-expr e filename (syntax->datum #'f))) (syntax->list #'(to-insert ...)))]
      [[(f ...) to-insert ...]
       (for-each (lambda (e) (interpret-insert-expr e filename 'with)) (syntax->list #'(to-insert ...)))]))
  
  (define (interpret-insert-expr stx filename scope-id)
    (syntax-case stx (on-entry on-exit)
      [[on-entry src-expr ...]
       (let ([table (hash-ref insert-table filename)]
             [insert-stx (quasisyntax/loc stx
                           (begin
                             #,@(map (lambda (e) (interpret-src-expr e)) (syntax->list #'(src-expr ...)))))])
         (hash-set! table scope-id (list (cons 'entry insert-stx))))] ; remember to append list, in the same function can have multiple places to insert
      [[on-exit src-expr ...]
       (for-each (lambda (e) (interpret-src-expr e)) (syntax->list #'(src-expr ...)))]
      [else
       (interpret-at-expr stx)]))
  
  (define (interpret-at-expr stx)
    (syntax-case stx (at each-expression)
      [[(at each-expression) border-expr ...]
       ; do something 
       (for-each (lambda (e) (interpret-border-expr e)) (syntax->list #'(border-expr ...)))]
      [[(at location-expr #:before expr ...) border-expr ...]
       (begin
         (interpret-location-expr #'location-expr)
         (for-each (lambda (e) (interpret-border-expr e)) (syntax->list #'(border-expr ...)))
         (for-each (lambda (e) (interpret-location-expr e)) (syntax->list #'(expr ...))))]
      [[(at location-expr #:after expr ...) border-expr ...]
       (begin
         (interpret-location-expr #'location-expr)
         (for-each (lambda (e) (interpret-border-expr e)) (syntax->list #'(border-expr ...)))
         (for-each (lambda (e) (interpret-location-expr e)) (syntax->list #'(expr ...))))]
      [[(at location-expr) border-expr ...]
       (begin
         (interpret-location-expr #'location-expr)
         (for-each (lambda (e) (interpret-border-expr e)) (syntax->list #'(border-expr ...))))]
      [else
       (error 'invalid-debugging-expression "expr = ~a\n" (syntax->datum stx))]))
  
  ; test ref syntax case, test with-start specitial symbol ||
  (define (interpret-location-expr stx)
    (syntax-case stx (ref with-start)
      [(ref src-id)
       (hash-ref src-env (syntax->datum #'src-id) 'src-env-lookup-failure)]
      [(with-start |part-of-expr|)
       (void)]
      [src-expr
       (void)]))   
  
  (define (interpret-fun-pattern stx)
    (syntax-case stx ()
      [(with-start |part-of-fun-name|)
       (void)]))
  
  (define (interpret-border-expr stx)
    (syntax-case stx (on-entry on-exit ref)
      [[on-entry (ref src-id)]
       (quasisyntax/loc stx [on-entry #,@(hash-ref src-env (syntax->datum #'src-id) 'src-env-lookup-failure)])]
      [[on-entry src-expr ...]
       (void)] ; to-do
      [[on-exit (ref src-id)]
       (quasisyntax/loc stx [on-exit #,@(hash-ref src-env (syntax->datum #'src-id) 'src-env-lookup-failure)])]
      [[on-exit src-expr ...]
       (void)])) ; to-doo
  
  (define (interpret-src-expr stx)
    (syntax-case stx (ref)
      [(ref src-id)
       (hash-ref src-env (syntax->datum #'src-id) 'src-env-lookup-failure)]
      [else
       stx]))
  
  (syntax-case stx (medic layer)
    [(medic (layer layer-id layer-expr ...) ...)
     (for-each (lambda (l) (interpret-layer-form l)) (syntax->list #'((layer layer-id layer-expr ...) ...)))]
    [else
     (error 'invalid-debugging-expression "expr = ~a\n" (syntax->datum stx))])
  
  insert-table)




(define example-stx (syntax (debug (layer layer1
                                          (def s1 #:src (define x 1) (define z 3))
                                          (in #:file test
                                              [within (ref s1)]
                                              [(f [on-entry (print x)])]))
                                   
                                   (layer layer2
                                          (in #:file test1
                                              [within (define y 1)]))
                                   )))

(define example-stx1 (syntax (debug (layer layer1 
                                           (in #:file factorial
                                               [(fact [on-entry (printf "x = ~a\n" x)])])))))

(define example-stx2 (syntax (medic
                              (layer layer1 
                                     (in #:file "factorial.rkt"
                                         [fact [on-entry (printf "x = ~a\n" x)
                                                         ]])))))


;(interpret example-stx2)