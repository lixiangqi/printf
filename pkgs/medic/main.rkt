#lang racket

(require "medic-structs.rkt")

(provide (rename-out [module-begin #%module-begin])
         define log
         #%top-interaction)


(module reader syntax/module-reader
  medic)

(define-syntax-rule (module-begin form ...)
  (#%module-begin 
   (provide medic-table)
   (define medic-table (interp #'(begin form ...)))))

(define (interp stx)
  
  (define counter 0)
  (define current-layer-id #f)
  (define exports '())
  (define imports '())
  (define import-table '())
  (define src-table (make-hash)) ; id -> list-of-syntax
  (define debug-table (make-hash))
  
  ;; global-env is for variables' lookup when layer importing or exporting
  ; map from layer identifier to env structure
  (define global-env (make-hash))
    
  ;; insert-table and at-inserts store the information about inserting expressions
  ;; to the source programs. 
  
  ; insert-table: map from complete path string to a hash table, which maps
  ; from scope-id to a list of pairs ((or 'exit 'entry), to-be-inserted-exprs)
  ; scope-id:
  ; - 'module
  ; - function name (string)
  ; - 'each-function
  ; - (cons 'start part-of-fun-name-string) 
  (define insert-table (make-hash))
  
  ; at-inserts: map from complete path string to a list of at-insert structure
  (define at-inserts (make-hash))
  
  ; template: map from complete path string to a hash table, which maps
  ; function name (symbol) to (list function-template-string args return-value)
  ; one function can only take one logging behavior in one debugging program
  (define template (make-hash))
  
  (define (interpret-layer-form stx)
    (syntax-case stx (layer)
      [(layer layer-id #:enable flag layer-expr ...) (identifier? #'layer-id)
       (begin
         (set! current-layer-id (syntax->datum #'layer-id))
         (set! exports '())
         (set! imports '())
         (set! import-table '())
         (set! src-table (make-hash))
         (set! debug-table (make-hash))
         (for-each (lambda (expr) 
                     (interpret-expr expr (syntax->datum #'flag)))
                   (syntax->list #'(layer-expr ...)))
         (hash-set! global-env current-layer-id (env exports imports import-table src-table debug-table)))]    
      [(layer layer-id layer-expr ...) (identifier? #'layer-id)
       (interpret-layer-form (syntax/loc stx (layer layer-id #:enable #t layer-expr ...)))]                                          
      [else
       (error 'invalid-medic-expression "expr = ~a\n" (syntax->datum stx))]))
  
  (define (interpret-expr stx flag)
    (syntax-case stx (def in)
      [(def src-id #:src src-expr ...) (identifier? #'src-id)
       (if (member (syntax->datum #'src-id) imports)
           (error 'conflicting-identifiers "identifier ~v in expr = ~v already imported" (syntax->datum #'src-id) (syntax->datum stx)) 
           (let ([expands (flatten (map interpret-src-expr (syntax->list #'(src-expr ...))))])
             (hash-set! src-table (syntax->datum #'src-id) expands)))]
      [(def debug-id #:debug expr ...) (identifier? #'debug-id) ; expr ... is lazy evaluated, may contain unexpanded (ref ...) form
       (hash-set! debug-table (syntax->datum #'debug-id) #'(expr ...))]
      [(in #:file id expr ...)
       (if flag
           (let ([fn (path->string 
                      (resolved-module-path-name 
                       ((current-module-name-resolver) (syntax->datum #'id) #f #f)))])
             (unless (hash-has-key? insert-table fn)
               (hash-set! insert-table fn (make-hash)))
             (unless (hash-has-key? template fn)
               (hash-set! template fn (make-hash)))
             (for-each (lambda (e)
                         (interpret-match-expr e fn))
                       (syntax->list #'(expr ...))))
           (list insert-table at-inserts template))]
      [(op id ...) (equal? 'import (syntax->datum #'op))
       (let ([imported-ids (map 
                             (lambda (id) 
                               (let* ([layer-id (syntax->datum id)]
                                      [exported (env-exports (hash-ref global-env layer-id))])
                                 (set! import-table (append import-table (list (cons layer-id exported))))
                                 exported))
                             (syntax->list #'(id ...)))])
         (set! imports (flatten imported-ids)))]
      [(op id ...) (equal? 'export (syntax->datum #'op))
       (set! exports (map syntax->datum (syntax->list #'(id ...))))] 
      [else
       (error 'invalid-medic-expression "expr = ~a\n" (syntax->datum stx))]))
  
  ; interpret-match-expr: syntax string-of-file-name -> void
  (define (interpret-match-expr stx fn)
    (syntax-case stx (ref at with-behavior each-function with-start each-expression)
      [(ref debug-id)
       (let* ([id (syntax->datum #'debug-id)]
              [expr (hash-ref debug-table id #f)])
         (cond
           [expr
            (for-each (lambda (e) (interpret-match-expr e fn)) (syntax->list expr))]
           [else
            (let iterate ([lst import-table])
              (when (null? lst) 
                (error 'refer-to-unbound-variable "id = ~a in expr = ~a\n" id (syntax->datum stx)))
              (if (member id (cdr (first lst)))
                  (let ([found-expr (hash-ref (env-debug-table (hash-ref global-env (car (first lst)))) id)])
                     (for-each (lambda (e) (interpret-match-expr e fn)) (syntax->list found-expr)))
                  (iterate (rest lst))))]))]
      
      [(with-behavior f s)
       (let* ([fun (syntax->datum #'f)]
              [str (format "~a" (syntax->datum #'s))]
              [at-args (remove-duplicates (regexp-match* #px"@\\w+" str))]
              [at-ret (remove-duplicates (regexp-match* #px"@,\\w+" str))]
              [ret (if (null? at-ret) #f (car at-ret))]
              [table (hash-ref template fn)])
         (hash-set! table fun (list str at-args ret)))]
      
      [[each-function to-insert ...]
       (for-each (lambda (e) (interpret-insert-expr e fn (list 'each-function))) (syntax->list #'(to-insert ...)))]
      
      [[(with-start part-of-fun-name) to-insert ...]
       (let ([str (format "~a" (syntax->datum #'part-of-fun-name))])
         (for-each (lambda (e) (interpret-insert-expr e fn (list 'with-start str))) (syntax->list #'(to-insert ...))))]
      
      [[(at expr ...) border-expr ...]
       (interpret-insert-expr stx fn (list 'module))]
      
      [[(f ...) to-insert ...]
       (let ([funs (map (lambda (f) (format "~a" (syntax->datum f))) (syntax->list #'(f ...)))])
         (for-each (lambda (e) 
                     (interpret-insert-expr e fn funs))
                   (syntax->list #'(to-insert ...))))]
      [else
       (interpret-insert-expr stx fn (list 'module))]))
  
  ; interpret-insert-expr: syntax string (list-of symbol) -> void
  (define (interpret-insert-expr stx fn scope-ids)
    (define (insert-expr loc inserts)
      (let ([table (hash-ref insert-table fn)])
        (if (equal? 'with-start (first scope-ids))
            (let* ([key (cons 'start (second scope-ids))]
                   [exist (hash-ref table key '())])
              (hash-set! table key (append exist (list (cons loc inserts)))))
            (for-each (lambda (i)
                        (let ([exist (hash-ref table i '())])
                          (hash-set! table i (append exist (list (cons loc inserts))))))
                   scope-ids))))
    
    (syntax-case stx (on-entry on-exit)
      [[on-entry src-expr ...]
       (insert-expr 'entry (map interpret-src-expr (syntax->list #'(src-expr ...))))]
      [[on-exit src-expr ...]
       (insert-expr 'exit (map interpret-src-expr (syntax->list #'(src-expr ...))))]
      [else
       (interpret-at-expr stx fn (map (lambda (i) (format "~a" i)) scope-ids))]))
  
  (define (interpret-at-expr stx fn scope)
    (define (interpret-location-expr stx)
      (syntax-case stx (with-start)
        [(with-start part-of-expr)
         (format "~a" (syntax->datum #'part-of-expr))]
        [else (format "~a" (syntax->datum stx))]))
    
    (syntax-case stx (at with-start)
      [[(at (with-start part-of-expr)) border-expr ...]
       (let ([str (format "~a" (syntax->datum #'part-of-expr))])
         (for-each (lambda (e) 
                     (interpret-border-expr e fn scope str)) 
                   (syntax->list #'(border-expr ...))))]
      
      [[(at location-expr [#:before expr1 ...] [#:after expr2 ...]) border-expr ...]
       (let ([target-exp (format "~a" (syntax->datum #'location-expr))]
             [before-exp (map interpret-location-expr (syntax->list #'(expr1 ...)))]
             [after-exp (map interpret-location-expr (syntax->list #'(expr2 ...)))])
         (for-each (lambda (e) 
                     (interpret-border-expr e fn scope target-exp before-exp after-exp)) 
                   (syntax->list #'(border-expr ...))))]
      
      [[(at location-expr [#:before expr ...]) border-expr ...]
       (let ([target-exp (format "~a" (syntax->datum #'location-expr))]
             [before-exp (map interpret-location-expr (syntax->list #'(expr ...)))])
         (for-each (lambda (e) 
                     (interpret-border-expr e fn scope target-exp before-exp)) 
                   (syntax->list #'(border-expr ...))))]
      
      [[(at location-expr [#:after expr ...]) border-expr ...]
       (let ([target-exp (format "~a" (syntax->datum #'location-expr))]
             [after-exp (map interpret-location-expr (syntax->list #'(expr ...)))])
         (for-each (lambda (e) 
                     (interpret-border-expr e fn scope target-exp '() after-exp)) 
                   (syntax->list #'(border-expr ...))))]
      
      [[(at location-expr) border-expr ...]
       (for-each (lambda (e) 
                   (interpret-border-expr e fn scope (format "~a" (syntax->datum #'location-expr)))) 
                 (syntax->list #'(border-expr ...)))]
      
      [else
       (error 'invalid-medic-expression "expr = ~a\n" (syntax->datum stx))]))
  
  (define (interpret-border-expr stx fn scope target-exp [before '()] [after '()])
    
    (define (add-at-insert s)
      (let ([exist (hash-ref at-inserts fn '())])
        (hash-set! at-inserts fn (append exist (list s)))))
    
    (syntax-case stx (on-entry on-exit ref)
      [[on-entry (ref src-id)]
       (let ([found (hash-ref src-table (syntax->datum #'src-id) #f)])
         (cond
           [found 
            (interpret-border-expr (syntax/loc stx [on-entry found]) fn scope target-exp before after)]
           [else
            (error 'refer-to-unbound-variable "id = ~a in expr = ~a\n" (syntax->datum #'src-id) (syntax->datum stx))]))]
      
      [[on-entry src-expr ...]
       (let* ([exprs (map interpret-src-expr (syntax->list #'(src-expr ...)))]
              [at-struct (at-insert scope target-exp before after 'entry exprs)])
         (add-at-insert at-struct))]
      
      [[on-exit (ref src-id)]
       (let ([found (hash-ref src-table (syntax->datum #'src-id) #f)])
         (cond
           [found 
            (interpret-border-expr (syntax/loc stx [on-exit found]) fn scope target-exp before after)]
           [else
            (error 'refer-to-unbound-variable "id = ~a in expr = ~a\n" (syntax->datum #'src-id) (syntax->datum stx))]))]
      
      [[on-exit src-expr ...]
       (let* ([exprs (map interpret-src-expr (syntax->list #'(src-expr ...)))]
              [at-struct (at-insert scope target-exp before after 'exit exprs)])
         (add-at-insert at-struct))]))
  
  (define (interpret-src-expr stx)
    
    (define (attach-stx-property s d)
      (set! counter (add1 counter))
      (define original-e (if (list? d) 
                             (map (lambda (i) (format "~a" (syntax->datum i))) d) 
                             (format "~a" (syntax->datum d))))
      (syntax-property (syntax-property s 'layer current-layer-id)
                       'stamp (cons counter original-e)))
      
    (syntax-case stx (ref aggregate timeline assert log same?)
      [(ref src-id)
       (let* ([id (syntax->datum #'src-id)]
              [exprs (hash-ref src-table id #f)])
         (cond
           [exprs 
            (map interpret-src-expr exprs)]
           [else
            (let iterate ([lst import-table])
              (when (null? lst)
                (error 'refer-to-unbound-variable "id = ~a in expr = ~a\n" id (syntax->datum stx)))
              (if (member id (cdr (first lst)))
                  (let ([found-expr (hash-ref (env-src-table (hash-ref global-env (car (first lst)))) id)])
                    (map interpret-src-expr found-expr))
                  (iterate (rest lst))))]))]
      
      [(log v)
       (attach-stx-property stx #'v)]
      
      [(aggregate) 
       (error 'invalid-medic-expression "expr = ~a\n" (syntax->datum stx))]
      
      [(aggregate v ...) 
       (attach-stx-property stx (syntax->list #'(v ...)))]
      
      [(same? id) (not (identifier? #'id))
       (error 'invalid-medic-expression "expr = ~a\n" (syntax->datum stx))]      
      
      [(timeline id)
       (attach-stx-property stx #'id)]
      
      [(assert cond)
       (attach-stx-property stx #'cond)]
      
      [(define (var) expr ...)
       (quasisyntax/loc stx
         (define (var)
           #,@(map interpret-src-expr (syntax->list #'(expr ...)))))]
      
      [else (syntax-property stx 'layer current-layer-id)]))
  
  (syntax-case stx (begin layer)
    [(begin (layer layer-id layer-expr ...) ...)
     (for-each (lambda (l) 
                 (interpret-layer-form l)) 
               (syntax->list #'((layer layer-id layer-expr ...) ...)))]
    [else
     (error 'invalid-medic-expression "expr = ~a\n" (syntax->datum stx))])
  (list insert-table at-inserts template))