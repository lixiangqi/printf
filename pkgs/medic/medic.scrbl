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
task of debugging and a full featured tracing library to enhances the traditional debugging
technique of inserting print-like expressions into the source program. 
@local-table-of-contents[]
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
              [(f f ...) insert-expr insert-expr ...]]
  [insert-expr border-expr
               at-expr]
  [border-expr [on-entry source-expr source-expr ...]
               [on-exit source-expr source-expr ...]]
  [at-expr [(at location-expr) border-expr border-expr ...]
              [(at location-expr before-expr) border-expr border-expr ...]
              [(at location-expr after-expr) border-expr border-expr ...]
              [(at location-expr before-expr after-expr) border-expr border-expr ...]]
  [location-expr target-language-expression
                 at-pattern-expr]
  [at-pattern-expr (with-start part-of-target-language-expression)]
  [fun-pattern-expr (with-start part-of-target-language-function-name)]
  [before-expr [#:before location-expr location-expr ...]]
  [after-expr [#:after location-expr location-expr ...]]
  [source-expr (ref debug-src-id)
               target-language-expression]
  [flag boolean]
  [template string]
  [part-of-target-language-expression string]
  [part-of-target-language-function-name string]
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
        behaviors is captured by @racket[match-expr], which involves @emph{where} to pinpoint and @emph{what} to do. 
        
        As the Medic language is intended to be @emph{target-language-independent} that works with most popular programming
        language as much as possible, a minimum set of scope categories is chosen: module scope and function scope. For 
        example, for the above grammar, the third clause of the @racket[match-expr] non-terminal is within module scope, 
        and the following three clauses are within function scope. Function scope can be function name exact matching or 
        pattern matching. The form @racket[(f f ...)] matches one or more function names enclosed in the parenthesis, and
        @racket[fun-patten-expr] matches a pattern of function names starting with some common characters, which are components
        of the string @racket[part-of-target-language-function-name] in @racket[(with-start part-of-target-language-function-name)].
        The debugging primitive @racket[each-function] supports refering to every function defined in 
        the module. 
        
        With clear scope declared for debugging, exact location descriptions are supported by @racket[border-expr] and 
        @racket[at-expr]. The goal of @racket[at-expr] is to facilitate accurately locating the target expression anywhere
        in the source program. The @racket[location-expr] expression in the form @racket[(at location-expr ....)] can be a 
        complete expression in the target program or a part of the expression represented by @racket[at-pattern-expr] when
        the expression is complicated. To avoid the confusions of multiple matches of @racket[location-expr] in the target
        program, specification of @racket[before-expr] and @racket[after-expr] can be employed to confine the lexical context 
        of the target expression @racket[location-expr]. If @racket[border-expr] is within @racket[at-expr], the debugging code 
        @racket[source-expr source-expr ...] is inserted before or after the source expression matched by @racket[at-expr];
        otherwise, it is inserted at the beginning or the end of a function or module. 
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

@section{Evaluation}

@defmodule[medic/core]

@defproc[(medic [path (or/c relative-path? complete-path?)] ...) void?]{
Evaluates the Medic programs and installs the debugging instructions into the target programs at compile time.
}

@defproc[(debug [path (or/c relative-path? complete-path?)]) void?]{
Evaluates the target program indicated by the file path @racket[path] and opens the Medic graphical interface
showing any debugging traces information.                                                        
}

@section{Using the Medic Debugger}
Debugging with the Medic debugger consists of three kinds of programs: source programs, Medic programs, and a 
debugging script. Medic programs represent debugging instructions about the source programs and a debugging 
script runs the Medic programs and starts debugging the source programs. After the evaluation of the debugging
script, a debugging graphical interface is presented, which consists of four panes: a Log pane, Graph pane, 
Aggregate pane and Timeline pane.

@subsection[#:tag "log"]{Tracing Log}
Like the traditional print-like expressions, the tracing @racket[log] produces a linear and textual debugging information
to identify problems in program execution. However, @racket[log] is more advanced than the traditional print-like expression in 
two ways:
@itemize[
  @item{Show the context.}
  @item{Show the behavior.}
  ]

Suppose the value of @racket[x] is 3 and we call @racket[(log x)]. Instead of merely printing out the value of @racket[x],
it prints out ``@racket[x] = 3'', which displays the extra context information of the value 3---it is the variable
@racket[x] that we are inspecting. 

All traditional print-like expressions are concerned with displaying values of data, but under some debugging 
circumstances, showing the behavior of data is needed. Consider the following example:

@racketblock[
(define (f x y)
  (+ (sqr x) (sqr y)))
]

When we call @racket[(log (f 3 4))], it produces a tracing log ``(@racket[f] 3 4) = 25'', which presents no information
about what the @racket[f] function does. To change the behavior of @racket[(log (f 3 4))], we can modify
the Medic program by adding @racket[(with-behavior f "Calling f: sum of @x squared and @y squared is @,ret")]. The @"@" notation
provides a way to obtain the values of arguments of a function as well as the function returning value. For example, the above
@"@"@racket[x] gets the value of @racket[x] and @"@"@racket[,ret] keeps the returning value of the @racket[f] function call. 
Then the call of @racket[(log (f 3 4))] generates ``Calling f: sum of 3 squared and 4 squared is 25''. The benefits of 
allowing @racket[log] to show the behavior of functions are that programmers have control over writing the descriptions
of functions and changing the description at one place can change all behaviors of related function calls at different
places.

@subsection{Tracing Graph}


@subsection{Aggregate View}

@subsection{Timeline View}

@section{Debugging Examples}

@subsection[#:style '(toc)]{Learning Medic Language}
@local-table-of-contents[]
@subsubsection{Demo 1}
Basic module-level and function-level insertion of some debugging code.

@bold{@tt{src1.rkt:}}
@codeblock{
#lang racket
(define z 2)
(define n 9)

(define (f x)
  (define z 5)
  (define n 4)
  (if (zero? x)
      1
      (* x (sub1 x))))

(f 3)
}

@bold{@tt{src1-medic.rkt:}}
@codeblock{
#lang medic
(layer layer1 
       (in #:file "src1.rkt"
           ; module-level border-expr
           [on-entry (define x 1)
                     (define y 2)]
           [on-exit 
            (log "module exit:")
            (log y)]
           ; module-level at-expr
           [(at (define n 9)) [on-exit (log "module at:")
                                       (log n)]]
           ; function-level at-expr and border-expr
           [(fact) 
            [(at (with-start "(* x (sub1")) [on-entry (log "else branch:") (log n)]]
            [on-entry (define y 30)
                      (log "function entry:")
                      (log x)
                      (log y)]
            [on-exit (log "function exit:")
                     (log n)]])) 
}
@subsubsection{Demo 2}
The @racket[at-expr] pattern matching with @racket[before-expr] and @racket[after-expr] specification.

@bold{@tt{src2.rkt:}}
@codeblock{
#lang racket

(define x 10)

(define counter 0)

(define (inc-counter) (set! counter (add1 counter)))

(define (inc x) 
  (inc-counter)
  (+ x 1))

(define (g)
  (define x (inc 4))
  (inc-counter)
  (+ x 1))

(g) 
}
@bold{@tt{src2-medic.rkt:}}
@codeblock{
#lang medic

(layer layer1 
       (in #:file "src.rkt"
           ; match two instances of (inc-counter)
           [(at (inc-counter)) [on-entry (log "[1]calling inc-counter")]]
           
           ; match two instances of (+ x 1)
           [(at (+ x 1) [#:before (inc-counter)]) [on-entry (log "[2]calling (+ x 1)")]]
           
           ; only match (+ x 1) in g function
           [(at (+ x 1) [#:before (define x (inc 4))
                                  (inc-counter)])
            [on-entry (log "[3]calling (+ x 1) in g")]]
           [(g) [(at (+ x 1)) [on-entry (log "[4]match (+ x 1) in g")]]]
           
           ; only match (inc-counter) in function g
           [(at (inc-counter) [#:before (define x (inc 4))] [#:after (+ x 1)])
            (on-entry (log "[5]calling (inc-counter) in g"))]
           [(at (inc-counter) [#:before (with-start "(define x (inc")] [#:after (+ x 1)])
            (on-entry (log "[6]use with-start matching (inc-counter) in g"))]))
}

@subsection{Debugging via the Tracing Library}

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