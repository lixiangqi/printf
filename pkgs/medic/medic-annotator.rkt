(module annotator scheme/base
  
  (require (prefix-in kernel: syntax/kerncase)
           gui-debugger/marks
           mzlib/etc
           (prefix-in srfi: srfi/1/search)
           (for-syntax scheme/base)
           (only-in mzscheme [apply plain-apply])
           )
  (provide annotate-stx)
  
  (define (disarm stx) (syntax-disarm stx code-insp))
  (define (rearm old new) (syntax-rearm new old))
  (define code-insp (variable-reference->module-declaration-inspector
                     (#%variable-reference)))
  
  (define (arglist-bindings arglist-stx)
    (syntax-case arglist-stx ()
      [var
       (identifier? arglist-stx)
       (list arglist-stx)]
      [(var ...)
       (syntax->list arglist-stx)]
      [(var . others)
       (cons #'var (arglist-bindings #'others))]))
  
  (define (annotate-stx stx insert-table)
    (printf "annotate-stx: insert-table=~v\n" insert-table)
    (define top-level-ids '())
    
    (define (add-top-level-id var)
      (set! top-level-ids (cons var top-level-ids)))
    
    (define (top-level-annotate stx)
      (kernel:kernel-syntax-case stx #f
                                 [(module identifier name mb)
                                  (module-annotate stx)]
                                 [else-stx
                                  (general-top-level-expr-iterator stx)]))
    
    (define (module-annotate stx)
      (syntax-case stx ()
        [(_ identifier name mb)
         (syntax-case (disarm #'mb) ()
           [(plain-module-begin . module-level-exprs)
            (with-syntax ([(module . _) stx])
              (quasisyntax/loc stx (module identifier name
                                     #,(rearm
                                        #'mb
                                        #`(plain-module-begin
                                           ;xiangqi
                                           #,(module-level-expr-iterator (expand #'(define y 2)) #t)
                                           #,@(map (lambda (e) (module-level-expr-iterator e))
                                                   (syntax->list #'module-level-exprs))
                                           )))))])]))
    
    (define (module-level-expr-iterator stx [medic? #f])
      (kernel:kernel-syntax-case
       stx #f
       [(#%provide . provide-specs)
        stx]
       [else-stx
        (general-top-level-expr-iterator stx medic?)]))
    
    (define (general-top-level-expr-iterator stx [medic? #f])
      (kernel:kernel-syntax-case
       stx #f
       [(define-values (var ...) expr)
        (begin
          (for-each (lambda (v)
                      (when (or medic? (syntax-original? v))
                        (add-top-level-id v)))
                    (syntax->list #'(var ...)))
        (quasisyntax/loc stx
          (define-values (var ...) #,(annotate #`expr '() (syntax->datum (car (syntax->list #'(var ...))))))))]
       [(define-syntaxes (var ...) expr)
        stx]
       [(begin-for-syntax . exprs)
        stx]
       [(begin . top-level-exprs)
        (quasisyntax/loc stx (begin #,@(map (lambda (expr)
                                              (module-level-expr-iterator expr))
                                            (syntax->list #'top-level-exprs))))]
       [(#%require . require-specs)
        stx]
       [(module . _)
        (module-annotate stx)]
       [(module* . _)
        (module-annotate stx)]
       [else
        (annotate stx '())]))
    
    (define (annotate expr bound-vars [id #f])
      
      (define (let/rec-values-annotator letrec?)
        (kernel:kernel-syntax-case
         (disarm expr) #f
         [(label (((var ...) rhs) ...) . bodies)
          (let* ([new-bindings (apply append
                                      (map syntax->list
                                           (syntax->list #`((var ...) ...))))]
                 [all-bindings (append new-bindings bound-vars)]
                 [new-rhs (map (lambda (expr)
                                 (annotate expr
                                           (if letrec? all-bindings bound-vars)))
                               (syntax->list #'(rhs ...)))]
                 [last-body (car (reverse (syntax->list #'bodies)))]
                 [all-but-last-body (reverse (cdr (reverse (syntax->list #'bodies))))]
                 [bodies (append (map (lambda (expr)
                                        (annotate expr all-bindings))
                                      all-but-last-body)
                                 (list (annotate
                                        last-body
                                        all-bindings)))])
            (with-syntax ([(new-rhs/trans ...) new-rhs])
              (quasisyntax/loc expr
                (label (((var ...) new-rhs/trans) ...)
                       #,@bodies))))]))
      
      (define (lambda-clause-annotator clause)
        (kernel:kernel-syntax-case
         clause #f
         [(arg-list . bodies)
          (let* ([new-bound-vars (arglist-bindings #'arg-list)]
                 [all-bound-vars (append new-bound-vars bound-vars)]
                 [new-bodies (map (lambda (e) (annotate e all-bound-vars)) (syntax->list #'bodies))]
                 [to-insert (hash-ref insert-table id #f)]) ; in the future to-insert is a list of pair of inserts
           ; (if (hash-has-key? insert-table 'each-function)
            
            
            (if (and id to-insert)
                (let* ([first (car to-insert)]
                       [locator (car first)]
                       [insert-expr (cdr first)]
                       [ins (datum->syntax (car new-bound-vars) '(printf "z =~a\n" z) #'bodies)] 
                                                           
                       #;[ins (datum->syntax (car new-bound-vars) (syntax->datum insert-expr) #'bodies)])
                  ;(printf "first vars=~a\n" (car new-bound-vars))
                  ;(printf "ins=~a\n" ins)
                  ;(printf "first expr=~a\n" (car new-bodies))
                  
                  (case locator
                    [(entry) (printf "content=~a\n" insert-expr)]
                    [else (error 'scope-table "unknown locator: ~a" locator)])
                  (quasisyntax/loc clause
                    (arg-list
                     (begin
                       ;(printf "x = ~a\n" 1)
                       ;#,ins
                       #,@new-bodies)))
                  )
                
                (quasisyntax/loc clause
                  (arg-list 
                   (printf "hehe\n")
                   #,@new-bodies))))]))
      
      (define annotated
        (rearm
         expr
         (kernel:kernel-syntax-case
          (disarm expr) #f
          [var-stx (identifier? (syntax var-stx))
                   expr]
          
          [(#%plain-lambda . clause)
           (quasisyntax/loc expr 
             (#%plain-lambda #,@(lambda-clause-annotator #'clause)))]
          
          [(case-lambda . clauses)
           (quasisyntax/loc expr
             (case-lambda #,@(map lambda-clause-annotator (syntax->list #'clauses))))]
          
          [(if test then else)
           (quasisyntax/loc expr (if #,(annotate #'test bound-vars)
                                     #,(annotate #'then bound-vars)
                                     #,(annotate #'else bound-vars)))]
          
          [(begin . bodies)
           (quasisyntax/loc expr (begin #,@(map (lambda (e) (annotate e bound-vars)) (syntax->list #'bodies))))]
          
          [(begin0 . bodies)
           (quasisyntax/loc expr (begin0 #,@(map (lambda (e) 
                                                   (annotate e bound-vars))
                                                 (syntax->list #'bodies))))]
          
          [(let-values . clause)
           (let/rec-values-annotator #f)]
          
          [(letrec-values . clause) 
           (let/rec-values-annotator #t)]
          
          [(set! var val)
           (quasisyntax/loc expr (set! var #,(annotate #`val bound-vars)))]
          
          [(quote _) expr]
          
          [(quote-syntax _) expr]
          
          [(with-continuation-mark key mark body)
           (quasisyntax/loc expr (with-continuation-mark key
                                   #,(annotate #'mark bound-vars)
                                   #,(annotate #'body bound-vars)))]
          
          [(#%plain-app . exprs)
           (let ([subexprs (map (lambda (e) 
                                  (annotate e bound-vars))
                                (syntax->list #'exprs))])
             (quasisyntax/loc expr (#%plain-app . #,subexprs)))]
          
          [(#%top . var) expr]
          [(#%variable-reference . _) expr]
          
          [else (error 'expr-syntax-object-iterator "unknown expr: ~a"
                       (syntax->datum expr))])))
      annotated)
    
    (values (top-level-annotate stx)))
  )


