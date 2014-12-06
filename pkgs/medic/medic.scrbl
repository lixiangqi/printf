#lang scribble/doc

@(require scribble/manual
          redex/pict
          scriblib/figure
          (for-label (except-in racket log)
                     racket/contract/base
                     medic/trace))

@title{Medic Debugger}
@author["Xiangqi Li"]

the medic debugger 

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
  [at-pattern-expr (with-start |part-of-racket-expression|)]
  [fun-pattern-expr (with-start |part-of-racket-function-name|)]
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


