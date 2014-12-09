#lang scribble/doc

@(require scribble/manual
          redex/pict
          scriblib/figure
          (for-label (except-in racket log export import)
                     racket/contract/base
                     medic/trace))

@title{Medic Debugger}
@author["Xiangqi Li"]



@section{Medic Grammar}
@(racketgrammar* 
  #:literals (layer export import def in with-behavior ref each-function
              on-entry on-exit at with-start)
  [top-level-form (layer-form layer-form ...)]        
  [layer-form (layer layer-id layer-expr ...)
              (layer layer-id #:enable flag layer-expr ...)]
  [layer-expr (export id id ...)
              (import id id ...)
              debug-expr]
  [debug-expr (def debug-src-id #:src source-expr source-expr ...)
              (def debug-id #:debug match-expr match-expr ...)
              (in #:file id match-expr match-expr ...)]
  [match-expr (with-behavior f template)
              (ref debug-id)
              insert-expr 
              [each-function insert-expr insert-expr ...]
              [fun-pattern-expr insert-expr insert-expr ...]
              [(f f ...) insert-expr]]
  [insert-expr border-expr
               regex-expr]
  [border-expr [on-entry source-expr source-expr ...]
               [on-exit source-expr source-expr ...]]
  [regex-expr [(at location-expr) border-expr border-expr ...]
              [(at location-expr before-expr) border-expr border-expr ...]
              [(at location-expr after-expr) border-expr border-expr ...]
              [(at location-expr before-expr after-expr) border-expr border-expr ...]]
  [location-expr racket-expression
                 at-pattern-expr]
  [at-pattern-expr (with-start | part-of-racket-expression |)]
  [fun-pattern-expr (with-start | part-of-racket-function-name |)]
  [before-expr [#:before location-expr location-expr ...]]
  [after-expr [#:after location-expr location-expr ...]]
  [source-expr (ref debug-src-id)
               racket-expression]
  [flag boolean]
  [template string]
  [f variable-not-otherwise-mentioned]
  [id variable-not-otherwise-mentioned]
  [layer-id variable-not-otherwise-mentioned]
  [debug-src-id variable-not-otherwise-mentioned]
  [debug-id variable-not-otherwise-mentioned])
 
@section{Medic Tracing}

@defmodule[medic/trace]

@defproc[(edge [from object?] [to object?] [edge-label any/c ""] [from-label any/c ""] [to-label any/c ""] [color (or/c string? #f) #f])

         void?]{
Generates an edge in the Graph pane connecting from @racket[from] to @racket[to].  
The optional arguments @racket[edge-label], @racket[from-label], @racket[to-label] set the label 
properties of the edge and two connected nodes. The color of the arrow head of the edge is specified
by @racket[color]. When @racket[color] is @racket[#f], it uses the default gray color.                                  
}

@section{Log}
@section{Tracing Graph}

Suppose we have a buggy implementation of the doubly linked list:

@racketblock[
(define node%
  (class object%
    (super-new)
    (init-field [datum 0])
    (field [next #f]
           [previous #f])))

(define doubly-linked-list%
  (class object%
    (field [head #f]
           [tail #f])
    (super-new)
    (define size 0)
    
    (define/public (initialize d)
      (set! head (new node% [datum d]))
      (set! tail head)
      (set! size 1))
    
    (define/public (element-at i)
      (when (or (> i (sub1 size)) (< i 0))
        (error 'element-at-invalid-argument))
      (define temp head)
      (let loop ()
        (when (not (zero? i))
          (set! temp (get-field next temp))
          (set! i (sub1 i))
          (loop)))
      (get-field datum temp))
    
    (define/public (get-size) size)
    
    (define/public (add d)
      (cond
        [(zero? size) (initialize d)]
        [else
         (define temp (new node% [datum d]))
         (set-field! previous temp tail)
         (set-field! next tail temp)
         (set! tail temp)
         (set! size (add1 size))]))
    
    (define/public (add-at i d)
      (when (or (< i 0) (> i size))
        (error 'add-invalid-arguments))
      (if (= i size)
          (add d)
          (cond
            [(zero? i)
             (define temp (new node% [datum d]))
             (set-field! next temp head)
             (set-field! previous head temp)
             (set! head temp)
             (set! size (add1 size))]
            [else
             (define temp (new node% [datum d]))
             (define p head)
             (for ([j (in-range i)])
               (set! p (get-field next p)))
             (set-field! next temp p)
             (define p-prev (get-field previous p))
             (set-field! previous temp p-prev)
             (set-field! next p-prev temp)
             (set-field! previous p temp)
             (set! size (add1 size))])))
    
    (define/public (remove i)
      (when (or (< i 0) (> i (sub1 size)))
        (error 'remove-invalid-argument))
      (cond
        [(zero? i)
         (define res (get-field datum head))
         (set! head (get-field next head))
         (if head
             (set-field! previous head #f)
             (set! tail #f))
         (set! size (sub1 size))
         res]
        [else
         (cond
           [(= i (sub1 size))
            (define res (get-field datum tail))
            (set! tail (get-field previous tail))
            (set-field! next tail #f)
            (set! size (sub1 size))
            res]
           [else
            (define temp head)
            (for ([j (in-range i)]) (set! temp (get-field next temp)))
            (define res (get-field datum temp))
            (define temp-prev (get-field previous temp))
            (define temp-next (get-field next temp))
            (set-field! next temp-prev temp-next)
            (set! size (sub1 size))
            res])]))))
]
