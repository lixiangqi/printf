(module annotator scheme/base
  
  (require (prefix-in kernel: syntax/kerncase)
           (for-syntax scheme/base)
           (only-in mzscheme [apply plain-apply])
           racket/string
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
    (printf "template=~v\n" template)
    (define top-level-ids '())
    (define args-table (make-hash))
    
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
        (begin
          (for-each add-top-level-id (syntax->list #'(var ...)))
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
                                           (if letrec? all-bindings bound-vars)
                                           id))
                               (syntax->list #'(rhs ...)))]
                 [last-body (car (reverse (syntax->list #'bodies)))]
                 [all-but-last-body (reverse (cdr (reverse (syntax->list #'bodies))))]
                 [bodies (append (map (lambda (expr)
                                        (annotate expr all-bindings id))
                                      all-but-last-body)
                                 (list (annotate
                                        last-body
                                        all-bindings
                                        id)))])
            (with-syntax ([(new-rhs/trans ...) new-rhs])
              (quasisyntax/loc expr
                (label (((var ...) new-rhs/trans) ...)
                       #,@bodies))))]))
      
      (define (lambda-clause-annotator clause)
        (kernel:kernel-syntax-case
         clause #f
         [(arg-list . bodies)
          (let* ([new-bound-vars (arglist-bindings #'arg-list)]
                 [arg-strs (map (lambda (v) (format "~a" (syntax->datum v))) new-bound-vars)]
                 [all-bound-vars (append new-bound-vars bound-vars)]
                 [new-bodies (map (lambda (e) (annotate e all-bound-vars id)) (syntax->list #'bodies))])
            (when (hash-has-key? template id)
              (hash-set! args-table id arg-strs))
            (quasisyntax/loc clause
                  (arg-list
                   #,@new-bodies)))]))
     
      (define (log-expression-annotator e layer-id)
        (define (lookup-var args vals var)
          (let loop ([i 0])
            (if (< i (length args))
                (if (equal? (list-ref args i) var)
                    (list-ref vals i)
                    (loop (add1 i)))
                (error 'unbound-var-in-log-behavior-statement))))
        
        (define (substitute-val str from to)
          (if (null? from)
              str
              (substitute-val (string-replace str (car from) (car to)) (cdr from) (cdr to))))
                 
        (syntax-case e ()
          [(id) (identifier? #'id)
           (quasisyntax/loc e (#,add-log (format "~a = ~v" 'id id) '#,layer-id #f))] 
          [(app) (equal? (syntax->datum (car (syntax->list #'app))) '#%app)
           (let* ([app-lst (syntax->list #'app)]
                  [fun (cadr app-lst)]
                  [args (cddr app-lst)]
                  [fun-name (syntax->datum fun)]
                  [fun-args (hash-ref args-table fun-name null)]
                  [v (hash-ref template fun-name #f)])
             (cond
               [(identifier? fun)
                (if v
                    (let ([template-str (car v)]
                          [template-at-args (cadr v)]
                          [template-ret (caddr v)])
                      (quasisyntax/loc e
                        (let* ([arg-values (list #,@args)]
                               [ret-value (format "~v" (apply #,fun arg-values))]
                               [replaces (map (lambda (a)
                                                (format "~v" (#,lookup-var (list #,@fun-args) arg-values (string-trim a "@"))))
                                              (list #,@template-at-args))]
                               [str (#,substitute-val #,template-str (list #,@template-at-args) replaces)]
                               [final-str (if #,template-ret (#,string-replace str #,template-ret ret-value) str)])
                          (#,add-log final-str '#,layer-id #t))))
                    (quasisyntax/loc e (#,add-log (format "~a = ~v" 'app app) '#,layer-id #f)))]
               [else
                (error 'log-expression-annotator "unknown expr: ~a"
                       (syntax->datum e))]))]))
        
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
      
      (define (get-syntax-property e key)
        (or (syntax-property e key)
            (syntax-property (cadr (syntax->list e)) key)))
          
      (define annotated
        (rearm
         expr
         (kernel:kernel-syntax-case*
          (disarm expr) #f (log aggregate edge timeline assert)
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
           (quasisyntax/loc expr (if #,(annotate #'test bound-vars id)
                                     #,(annotate #'then bound-vars id)
                                     #,(annotate #'else bound-vars id)))]
          
          [(begin . bodies)
           (quasisyntax/loc expr (begin #,@(map (lambda (e) (annotate e bound-vars id)) (syntax->list #'bodies))))]
          
          [(begin0 . bodies)
           (quasisyntax/loc expr (begin0 #,@(map (lambda (e) (annotate e bound-vars id)) (syntax->list #'bodies))))]
          
          [(let-values . clause)
           (let/rec-values-annotator #f)]
          
          [(letrec-values . clause) 
           (let/rec-values-annotator #t)]
          
          [(set! var val)
           (quasisyntax/loc expr (set! var #,(annotate #`val bound-vars id)))]
         
          [(quote _) expr]
          
          [(quote-syntax _) expr]
          
          [(with-continuation-mark key mark body)
           (quasisyntax/loc expr (with-continuation-mark key
                                   #,(annotate #'mark bound-vars id)
                                   #,(annotate #'body bound-vars id)))]
          
          [(#%plain-app log . data)
           (log-expression-annotator #'data (get-syntax-property expr 'layer))]
          
          [(#%plain-app aggregate v ...)
           (let ([stamp-id (get-syntax-property expr 'stamp)])
             (quasisyntax/loc expr
               (#,record-aggregate #,stamp-id (list (cons 'v v) ...))))]
          
          [(#%plain-app edge . args)
           (edge-expression-annotator #'args)]
          
          [(#%plain-app timeline id) (identifier? (syntax id))
           (let ([timeline-id (get-syntax-property expr 'stamp)])
             (quasisyntax/loc expr
               (#%plain-app #,record-timeline #,timeline-id 'id id #f)))]
          
          [(#%plain-app assert cond)
           (let* ([stamp-id (get-syntax-property expr 'stamp)]
                  [id (car stamp-id)]
                  [label (cdr stamp-id)])
             (quasisyntax/loc expr
               (#%plain-app #,record-timeline #,id #,label cond #t)))]
                        
          [(#%plain-app . exprs)
           (let ([subexprs (map (lambda (expr) 
                                  (annotate expr bound-vars id))
                                (syntax->list #'exprs))])
             (quasisyntax/loc expr
               (#%plain-app . #,subexprs)))]
          
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
