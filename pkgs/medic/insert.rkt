(module annotator scheme/base
  
  (require (prefix-in kernel: syntax/kerncase)
           racket/list
           (for-syntax scheme/base)
           (only-in mzscheme [apply plain-apply])
           syntax/strip-context
           "medic-structs.rkt")
  
  (provide insert-stx)

  (define (insert-stx stx insert-table at-table)
    
    (define (convert-stx s) 
      (let* ([new-stx (strip-context s)]
             [tagged 
              (if (syntax->list new-stx)
                  (map (lambda (i) 
                         (if (identifier? i) 
                             (syntax-property i 'medic #t)
                             i)) 
                       (syntax->list new-stx))
                  new-stx)])
        (datum->syntax #f tagged s s)))
    
    ; when local? is #t, match at-pattern expression within function scope
    (define (match-at-table expr local? [id #f])
      (define ret #f)
      (define to-remove-entries '())
      (define pos (syntax-position expr))
      (when pos 
        (let iterate ([lst at-table]
                      [result-stx expr])
          (if (null? lst)
              (unless (equal? result-stx expr) (set! ret result-stx))
              (let* ([entry (first lst)]
                     [at-posns (finer-at-insert-posns entry)]
                     [insert-exprs (finer-at-insert-exprs entry)]
                     [scope (finer-at-insert-scope entry)]
                     [predicate (if local?
                                    (and (member id scope) (member pos at-posns))
                                    (member pos at-posns))])
                (if predicate
                    (begin
                      (cond
                        [(or (not local?) (equal? (length scope) 1))
                         (if (equal? (length at-posns) 1)
                             (set! to-remove-entries (append to-remove-entries (list entry)))
                             (set-finer-at-insert-posns! entry (remove pos at-posns)))]
                        [(and local? (not (equal? (length scope) 1)))
                         (set-finer-at-insert-posns! entry (remove pos at-posns))
                         (set-finer-at-insert-scope! entry (remove id scope))])
                      (case (finer-at-insert-loc entry)
                        [(entry) 
                         (iterate (rest lst)
                                  (syntax-property (quasisyntax/loc expr (begin #,@(map convert-stx insert-exprs)
                                                                                #,result-stx))
                                                   'debug #t))]
                        [(exit)
                         (iterate (rest lst)
                                  (syntax-property (quasisyntax/loc expr (begin #,result-stx
                                                                                 #,@(map convert-stx insert-exprs)))
                                                   'debug #t))]))
                    (iterate (rest lst) result-stx))))))
      (for-each (lambda (e) 
                  (set! at-table (remove e at-table)))
                to-remove-entries)
      ret)
    
    (define (match-border-insert scope loc) 
      (define inserts (hash-ref insert-table scope '()))
      (define result-stx '())
      (unless (null? inserts)
        (for-each (lambda (entry)
                    (when (equal? (car entry) loc)
                      (set! result-stx (append (map convert-stx (cdr entry)) result-stx))))
                  inserts))
      result-stx)
    
    (define (top-level-insert stx)
      (kernel:kernel-syntax-case stx #f
                                 [(module identifier name mb)
                                  (module-insert stx)]
                                 [else-stx
                                  (general-top-level-expr-iterator stx)]))
 
    (define (module-insert stx)
      (syntax-case stx ()
        [(_ identifier name mb)
         (syntax-case (disarm #'mb) ()
           [(plain-module-begin . module-level-exprs)
            (with-syntax ([(module . _) stx])
              (let* ([entry-res (match-border-insert 'module 'entry)]
                     [entry-exprs (if (null? entry-res) (list #'(#%plain-app void)) entry-res)]
                     [exit-exprs (match-border-insert 'module 'exit)])
                (hash-remove! insert-table 'module)
                (cond
                  [(null? exit-exprs)
                   (quasisyntax/loc stx (module identifier name
                                       #,(rearm
                                          #'mb
                                          #`(plain-module-begin
                                             
                                             #,(datum->syntax #f '(#%require medic/log
                                                                             medic/edge))
                                             #,@entry-exprs
                                             #,@(map (lambda (e) (module-level-expr-iterator e))
                                                     (syntax->list #'module-level-exprs))))))]
                  [else
                   (quasisyntax/loc stx (module identifier name
                                       #,(rearm
                                          #'mb
                                          #`(plain-module-begin
                                             #,(datum->syntax #f '(#%require medic/log
                                                                             medic/edge))
                                             #,@entry-exprs
                                             #,@(map (lambda (e) (module-level-expr-iterator e))
                                                     (syntax->list #'module-level-exprs))
                                             #,@exit-exprs))))])))])]))
    
  
    (define (module-level-expr-iterator stx)
      (kernel:kernel-syntax-case
       stx #f
       [(#%provide . provide-specs)
        stx]
       [else-stx
        (general-top-level-expr-iterator stx)]))
    
    (define (general-top-level-expr-iterator stx)
      
      (define ret (match-at-table stx #f))
          
      (or ret 
          (kernel:kernel-syntax-case
           stx #f
           [(define-values (var ...) expr)
            (quasisyntax/loc stx
              (define-values (var ...) #,(insert #`expr (format "~a" (syntax->datum (car (syntax->list #'(var ...))))))))]
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
            (module-insert stx)]
           [(module* . _)
            (module-insert stx)]
           [else
            (insert stx)])))
    
    (define (insert expr [id #f])
      
      (define ret (match-at-table expr #t id))
      
      (define (get-lambda-exit-entry-inserts id)
        (define entry-exprs (list #'(void)))
        (define exit-exprs '())
        (when id
          (let ([entry-res (match-border-insert id 'entry)])
            (set! entry-exprs (if (null? entry-res) (list #'(void)) entry-res))
            (set! exit-exprs (match-border-insert id 'exit))
            (hash-remove! insert-table id)
            (when (hash-has-key? insert-table 'each-function)
              (set! entry-exprs (append (match-border-insert 'each-function 'entry) entry-exprs))
              (set! exit-exprs (append (match-border-insert 'each-function 'exit) exit-exprs)))
            (let ([start-lst (filter (lambda (p) (and (pair? (car p)) (equal? (caar p) 'start)))
                                     (hash->list insert-table))])
              (for-each 
               (lambda (p)
                 (when (regexp-match (string-append "^" (cdar p)) id)
                   (set! entry-exprs (append (match-border-insert (car p) 'entry) entry-exprs))
                   (set! exit-exprs (append (match-border-insert (car p) 'exit) exit-exprs))))
               start-lst))))
        (values entry-exprs exit-exprs))
      
      (define (traverse exp body)
        (if (syntax-property exp 'debug)
            (let ([enriched-lst (syntax->list exp)]
                  [plain-lst (syntax->list body)]
                  [ret '()])
              (for ([j (in-range (length enriched-lst))])
                (let* ([layer-prop (syntax-property (list-ref enriched-lst j) 'layer)]
                       [ele (list-ref plain-lst j)]
                       [attached 
                        (if (and layer-prop (syntax->list ele))
                            (map (lambda (i)
                                   (if (identifier? i)
                                       (syntax-property (syntax-property i 'medic #t) 'layer layer-prop)
                                       i))
                                 (syntax->list ele))
                            ele)])
                  (set! ret (append ret (list attached)))))
              ret)
            (let ([decomposed-exp (syntax->list exp)]
                  [decomposed-body (syntax->list body)])
              (if decomposed-exp
                  (map (lambda (e b) (traverse e b)) decomposed-exp decomposed-body)
                  body))))
      
      (define (let/rec-values-annotator letrec?)
        (kernel:kernel-syntax-case
         (disarm expr) #f
         [(label (((var ...) rhs) ...) . bodies)
          (let ([new-rhs (map (lambda (e) (insert e id)) (syntax->list #'(rhs ...)))]
                [bodies (map (lambda (e) (insert e id)) (syntax->list #'bodies))])
            (with-syntax ([(new-rhs/trans ...) new-rhs])
              (quasisyntax/loc expr
                (label (((var ...) new-rhs/trans) ...)
                       #,@bodies))))]))
      
      (define (lambda-clause-annotator clause)
        (kernel:kernel-syntax-case
         clause #f
         [(arg-list . bodies)
          (begin
            (define new-bodies (map (lambda (e) (insert e id)) (syntax->list #'bodies)))
            (define-values (entry-exprs exit-exprs) (get-lambda-exit-entry-inserts id))
            (define arg-datum (syntax->datum #'arg-list))
            (define entry-datum (map syntax->datum entry-exprs))
            (define exit-datum (map syntax->datum exit-exprs))
            (define new-bodies-datum (map syntax->datum new-bodies))
            (define return-stx 
              (cond
                [(null? exit-exprs)
                 (datum->syntax #f (quasiquote (,arg-datum
                                                ,@entry-datum
                                                ,@new-bodies-datum)))]
                [else
                 (datum->syntax #f (quasiquote (,arg-datum
                                                ,@entry-datum
                                                ,@new-bodies-datum
                                                ,@exit-datum)))]))
            (define return-lst (syntax->list return-stx))
            (define body-index 1)
            
            (define (attach-stx-property exprs)
              (map (lambda (exp)
                     (define layer-prop (syntax-property exp 'layer))
                     (define body (list-ref return-lst body-index))
                     (set! body-index (add1 body-index))
                     (if (syntax->list body)
                         (map (lambda (i) 
                                (if (identifier? i)
                                    (syntax-property (syntax-property i 'medic #t) 'layer layer-prop)
                                    i)) 
                              (syntax->list body))
                         body))
                   exprs))
            
            (define attached-entries (attach-stx-property entry-exprs))
            (define attached-bodies
              (map (lambda (exp)
                     (define body (list-ref return-lst body-index))
                     (set! body-index (add1 body-index))
                     (traverse exp body))
                   new-bodies))
            (define attached-exits (attach-stx-property exit-exprs))
          
            (set! return-stx (datum->syntax #f (append (list (first return-lst))
                                                       attached-entries
                                                       attached-bodies
                                                       attached-exits)))
            return-stx)]))
    
      (or ret
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
             (quasisyntax/loc expr (if #,(insert #'test id)
                                       #,(insert #'then id)
                                       #,(insert #'else id)))]
          
            [(begin . bodies)
             (quasisyntax/loc expr (begin #,@(map (lambda (e) (insert e id)) (syntax->list #'bodies))))]
          
            [(begin0 . bodies)
             (quasisyntax/loc expr (begin0 #,@(map (lambda (e) (insert e id)) (syntax->list #'bodies))))]
          
            [(let-values . clause)
             (let/rec-values-annotator #f)]
          
            [(letrec-values . clause) 
             (let/rec-values-annotator #t)]
          
            [(set! var val)
             (quasisyntax/loc expr (set! var #,(insert #`val id)))]
          
            [(quote _) expr]
          
            [(quote-syntax _) expr]
          
            [(with-continuation-mark key mark body)
             (quasisyntax/loc expr (with-continuation-mark key
                                     #,(insert #'mark id)
                                     #,(insert #'body id)))]
          
            [(#%plain-app . exprs)
             (let ([subexprs (map (lambda (e) (insert e id)) (syntax->list #'exprs))])
               (quasisyntax/loc expr (#%plain-app . #,subexprs)))]
          
            [(#%top . var) expr]
            [(#%variable-reference . _) expr]
            
            [else (error 'expr-syntax-object-iterator "unknown expr: ~a"
                         (syntax->datum expr))]))))
    
    (top-level-insert stx))
  
  (define (disarm stx) (syntax-disarm stx code-insp))
  (define (rearm old new) (syntax-rearm new old))
  (define code-insp (variable-reference->module-declaration-inspector
                     (#%variable-reference))))