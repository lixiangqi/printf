(module annotator scheme/base
  
  (require (prefix-in kernel: syntax/kerncase)
           (for-syntax scheme/base)
           (only-in mzscheme [apply plain-apply])
           "redirect.rkt"
           "visual-util.rkt"
           "visual-lib.rkt")
  (provide annotate-stx)
  
  (define (arglist-bindings arglist-stx)
    (syntax-case arglist-stx ()
      [var
       (identifier? arglist-stx)
       (list arglist-stx)]
      [(var ...)
       (syntax->list arglist-stx)]
      [(var . others)
       (cons #'var (arglist-bindings #'others))]))
  
  (define (annotate-stx stx template)
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
                                           #,@(map (lambda (e) (module-level-expr-iterator e))
                                                   (syntax->list #'module-level-exprs)))))))])]))
    
    (define (module-level-expr-iterator stx)
      (kernel:kernel-syntax-case
       stx #f
       [(#%provide . provide-specs)
        stx]
       [else-stx
        (general-top-level-expr-iterator stx)]))
    
    (define (general-top-level-expr-iterator stx)
      
      (kernel:kernel-syntax-case
       stx #f
       [(define-values (var ...) expr)
        (let ([prop (syntax-property stx 'layer)])
          (for-each add-top-level-id (syntax->list #'(var ...)))
          (quasisyntax/loc stx
            (define-values (var ...) #,(annotate #`expr '() prop (syntax->datum (car (syntax->list #'(var ...))))))))]
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
    
    (define (annotate expr bound-vars [id-layer #f] [id #f])
      
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
                                           (if letrec? all-bindings bound-vars)
                                           id-layer id))
                               (syntax->list #'(rhs ...)))]
                 [last-body (car (reverse (syntax->list #'bodies)))]
                 [all-but-last-body (reverse (cdr (reverse (syntax->list #'bodies))))]
                 [bodies (append (map (lambda (expr)
                                        (annotate expr all-bindings id-layer id))
                                      all-but-last-body)
                                 (list (annotate
                                        last-body
                                        all-bindings
                                        id-layer id)))])
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
                 [new-bodies (map (lambda (e) (annotate e all-bound-vars id-layer id)) (syntax->list #'bodies))])
            #;(when id
              (printf "new bode=~v\n" new-bodies)
              (printf "res...=~v\n" (hash-ref template id)))
            (quasisyntax/loc clause
                  (arg-list
                  ; (printf "lambda entered.exp=~v\n" (list #,@new-bodies))
                   #,@new-bodies)))]))
      
      (define (edge-expression-annotator e)
        (syntax-case e ()
          [(from to)
           (quasisyntax/loc e
             (#%plain-app #,add-edge from to #f (format "~v" from) (format "~v" to)))]
          [(from to edge-label)
           (quasisyntax/loc e
             (#%plain-app #,add-edge from to edge-label (format "~v" from) (format "~v" to)))]
          [(from to edge-label from-label)
           (quasisyntax/loc e
             (#%plain-app #,add-edge from to edge-label from-label (format "~v" to)))]
          [(from to edge-label from-label to-label)
           (quasisyntax/loc e
             (#%plain-app #,add-edge from to edge-label from-label to-label))]))
      
      (define (find-bound-var/wrap-context var lst)
        (define (find-bound-var var-lst)
          (define ret #f)
          (let iterate ([vlst var-lst])
            (unless (null? vlst)
              (let ([bound (car vlst)])
                (if (equal? (syntax->datum var) (syntax->datum bound))
                    (set! ret (datum->syntax bound (syntax->datum var)))
                    (iterate (cdr vlst))))))
          ret)
        
        (define ret (find-bound-var lst))
        (unless ret
          (set! ret (find-bound-var top-level-ids)))
        ret)
          
      (define annotated
        (rearm
         expr
         (kernel:kernel-syntax-case*
          (disarm expr) #f (edge timeline assert)
          [var-stx (identifier? (syntax var-stx))
                   (if (syntax-property #'var-stx 'medic)
                       (or (find-bound-var/wrap-context #'var-stx bound-vars) expr)
                       expr)]
          
          [(#%plain-lambda . clause)
           (quasisyntax/loc expr 
             (#%plain-lambda #,@(lambda-clause-annotator #'clause)))]
          
          [(case-lambda . clauses)
           (quasisyntax/loc expr
             (case-lambda #,@(map lambda-clause-annotator (syntax->list #'clauses))))]
          
          [(if test then else)
           (quasisyntax/loc expr (if #,(annotate #'test bound-vars id-layer id)
                                     #,(annotate #'then bound-vars id-layer id)
                                     #,(annotate #'else bound-vars id-layer id)))]
          
          [(begin . bodies)
           (quasisyntax/loc expr (begin #,@(map (lambda (e) (annotate e bound-vars id-layer id)) (syntax->list #'bodies))))]
          
          [(begin0 . bodies)
           (quasisyntax/loc expr (begin0 #,@(map (lambda (e) (annotate e bound-vars id-layer id)) (syntax->list #'bodies))))]
          
          [(let-values . clause)
           (let/rec-values-annotator #f)]
          
          [(letrec-values . clause) 
           (let/rec-values-annotator #t)]
          
          [(set! var val)
           (quasisyntax/loc expr (set! var #,(annotate #`val bound-vars id-layer id)))]
         
          [(quote _) expr]
          
          [(quote-syntax _) expr]
          
          [(with-continuation-mark key mark body)
           (quasisyntax/loc expr (with-continuation-mark key
                                   #,(annotate #'mark bound-vars id-layer id)
                                   #,(annotate #'body bound-vars id-layer id)))]
          
          [(#%plain-app edge . args)
           (edge-expression-annotator #'args)]
          
          [(#%plain-app timeline id) 
           (let ([timeline-id (or (syntax-property expr 'timeline-id)
                                  (syntax-property (cadr (syntax->list expr)) 'timeline-id))])
             (printf "timeline-id=~v\n" timeline-id)
           expr)
;           (quasisyntax/loc expr
;             (#%plain-app #,record-timeline-data 
;             
;           (timeline-annotator #'id #f)
           
           ]
          
          [(#%plain-app assert cond)
           (let ([timeline-id (or (syntax-property expr 'timeline-id)
                                  (syntax-property (cadr (syntax->list expr)) 'timeline-id))])
             (printf "assert-id=~v\n" timeline-id)
           expr)
           
;           (timeline-annotator #'cond #t)
           ]
                        
          [(#%plain-app . exprs)
           (begin
             (define exprs-lst (syntax->list #'exprs))
             (cond
               [(equal? (syntax->datum (car exprs-lst)) 'edge)
                (edge-expression-annotator #'exprs)]
               [else
                (define layer #f)
                (define subexprs (map (lambda (e) 
                                        (when (equal? (syntax->datum e) 'printf)
                                          (set! layer (syntax-property e 'layer))
                                          (when (and id-layer (not layer))
                                            (set! layer id-layer))
                                          (unless layer
                                            (set! layer (syntax-property expr 'layer))))
                                        (annotate e bound-vars id-layer id))
                                      exprs-lst))
                (define port (set-up-output-port))
                (if layer
                    (quasisyntax/loc expr
                      (parameterize ([current-output-port #,port])
                        (#%plain-app #,add-layer-id '#,layer)
                        (#%plain-app . #,subexprs)))
                    (quasisyntax/loc expr
                      (#%plain-app . #,subexprs)))]))]
          
          [(#%top . var) expr]
          [(#%variable-reference . _) expr]
          
          [else (error 'expr-syntax-object-iterator "unknown expr: ~a"
                       (syntax->datum expr))])))
      annotated)
    
    (top-level-annotate stx))
  
  (define (disarm stx) (syntax-disarm stx code-insp))
  (define (rearm old new) (syntax-rearm new old))
  (define code-insp (variable-reference->module-declaration-inspector
                     (#%variable-reference))))
