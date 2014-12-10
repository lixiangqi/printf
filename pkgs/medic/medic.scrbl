#lang scribble/doc

@(require scribble/manual
          redex/pict
          scriblib/figure
          (for-label (except-in racket log export import)
                     racket/contract/base
                     medic/trace))

@title{Medic Debugger}
@author["Xiangqi Li"]

The Medic debugger is a debugging tool that incorporates a metaprogramming language to describe the
task of debugging and a full featured tracing library that enhances the traditional debugging
technique of inserting print-like expressions. 

@section{A Metaprogramming Language}

The Medic debugger treats the debugging as a metaprogramming activity, where the programmer writes
a debugging program about the target program to make invisible states of the source program visible.
The separation of a debugging program from a source program enables reusability and programmability
of the debugging program as well as the intactness of the source program. The debugging program can 
serve as a form of documentation, which preserves
the efforts invested in debugging, and act as something akin to testing suites that run against
a modified program later on during the process of software development.

Here is the grammar for the Medic metaprogramming language:

@(racketgrammar* 
  #:literals (layer export import def in with-behavior ref each-function
              on-entry on-exit at with-start)
  [top-level-form (layer-form layer-form ...)]        
  [layer-form (layer layer-id layer-expr ...)
              (layer layer-id #:enable flag layer-expr ...)]
  [layer-expr (export id id ...)
              (import layer-id layer-id ...)
              debug-expr]
  [debug-expr (def debug-src-id #:src source-expr source-expr ...)
              (def debug-id #:debug match-expr match-expr ...)
              (in #:file module-path match-expr match-expr ...)]
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
  [at-pattern-expr (with-start part-of-racket-expression)]
  [fun-pattern-expr (with-start part-of-racket-function-name)]
  [before-expr [#:before location-expr location-expr ...]]
  [after-expr [#:after location-expr location-expr ...]]
  [source-expr (ref debug-src-id)
               racket-expression]
  [flag boolean]
  [template string]
  [part-of-racket-expression string]
  [part-of-racket-function-name string]
  [f variable-not-otherwise-mentioned]
  [id variable-not-otherwise-mentioned]
  [layer-id variable-not-otherwise-mentioned]
  [debug-src-id variable-not-otherwise-mentioned]
  [debug-id variable-not-otherwise-mentioned])

There are some points about the language worth noting:

@itemize[
  @item{With the primitive elements of debugging and means of combination and abstraction 
        in this language, the Medic language provides the programmer with expressive powers over augmenting 
        the source program with desirable debugging behaviors without changing the source 
        program.}
  @item{The @racket[#:enable] keyword in @racket[layer-form] permits enabling and disabling adding
        the debugging behaviors described within @racket[layer-form] to the source code, while the debugging definitions
        within the layer are still available to other layers.}
  @item{The forms @racket[(export id id ...)] and @racket[(import layer-id layer-id ...)] declare
        exports and imports of a layer, where the @racket[id] is the identifier of an internal
        layer definition and @racket[layer-id] is some layer identifier.}   
  @item{The internal layer definitions supports code abstraction and reusability.
        The identifier @racket[debug-src-id] in the form @racket[(def debug-src-id #:src source-expr source-expr ...)] refers
        to a sequence of source expressions following after the @racket[#:src] keyword, and the
        expression @racket[(ref debug-src-id)] returns the corresponding source expressions. In a similar way, the
        identifier @racket[debug-id] in the form @racket[(def debug-id #:debug match-expr match-expr ...)] is bound
        to a sequence of debugging expressions and the expression @racket[(ref debug-id)] returns the corresponding
        debugging expressions.} 
  @item{In the form @racket[(in #:file module-path match-expr match-expr ...)], the specification for @racket[module-path] 
        can be three kinds of paths: a relative path, an absolute path, or a library path. For example, the following is
        acceptable specification for @racket[module-path].
        @racketblock[
        (code:comment "a relative path")
        (in #:file "src.rkt" ....)
        (code:comment "an absolute path")
        (in #:file (file "/home/xiangqi/test/src.rkt" ....))
        (code:comment "a library path")
        (in #:file test/src ....)
        ]} 
  @item{The form @racket[(with-behavior f template)] defines the behavior of the @racket[f] function, which is only useful
        when it goes with the tracing @racket[log] function. See @secref["log"] for more information about the usage.}
  @item{The core of the language's power to describe how the source program should exhibit the desirable debugging
        behaviors is captured by @racket[match-expr], which involves @emph{where} to pinpoint and @emph{what} to do. As
        the Medic language is intended to be @emph{target-language-independent} that works with most popular programming
        language as much as possible, a minimum set of scope categories is chosen: module scope and function scope. For 
        example, for the above grammar, the third clause of the @racket[match-expr] non-terminal is within module scope, 
        and the following three clauses are within function scope. pattern matching function scope
        
        
        The Medic debugging language designs a function scope
        primitive @racket[each-function]
        
        entry, exit, pattern matching, 
        function scope primitive,
        
        }
 
                                                              

]
 
@section{Medic Tracing}

@defmodule[medic/trace]

@defproc[(log [datum any/c]) void?]{
Adds a log entry in the Log pane.
}

@defproc[(edge [from object?] [to object?] [edge-label any/c ""] [from-label any/c ""] [to-label any/c ""] [color (or/c string? #f) #f])
         void?]{
Generates an edge in the Graph pane connecting from @racket[from] to @racket[to].  
The optional arguments @racket[edge-label], @racket[from-label], @racket[to-label] set the label 
properties of the edge and two connected nodes. The color of the arrow head of the edge is specified
by @racket[color]. When @racket[color] is @racket[#f], it uses the default gray color.                                  
}

@defproc[(aggregate [v any/c] ...) void?]{
Adds an aggregate entry in the Aggregate pane, which groups a sequence of @racket[v] together.
}

@defproc[(timeline [v any/c]) void?]{
Adds a timeline entry in the Timeline pane, with a sequence of the value of @racket[v] ordered in time.
}

@defproc[(assert [pred boolean?]) void?]{
Adds a timeline entry in the Timeline pane, where the violations of the invariant @racket[pred] are
highlighted in red color.                                                                                  
}

@defproc[(same? [v any/c]) void?]{
Adds a timeline entry in the Timeline pane. It checks whether the value of @racket[v] changes over time,
where @racket[v] can be a primitive data type such as a number, string, or symbol or a compound data 
type including a pair, vector, hash table, structure, and class. For a compound data type, a change to 
an instance of the data type is defined as a change to any part of an instance of the data type. A change 
to an object of a class is defined to be a change to any of the object's inherited, private, or public member
fields.                                                                        
}
@section[#:tag "log"]{Tracing Log}

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

@section{Aggregate View}

@section{Timeline View}