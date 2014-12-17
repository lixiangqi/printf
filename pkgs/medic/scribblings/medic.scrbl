#lang scribble/doc

@(require scribble/manual
          redex/pict
          scriblib/figure
          (for-label (except-in racket log export import remove)
                     racket/contract/base
                     medic/trace))

@title{Medic Debugger}
@author["Xiangqi Li"]

The Medic debugger is a debugging tool that incorporates a metaprogramming language to describe the
task of debugging and a full featured tracing library to enhances the traditional debugging
technique of inserting print-like expressions into the source program. 
@local-table-of-contents[]
@section{A Metaprogramming Language}
@defmodulelang[medic]
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
  @item{The @racketvarfont{layer-form} form modularizes debugging code and facilitates organizing debugging traces into
        different layers.
        The @racket[#:enable] keyword in @racketvarfont{layer-form} permits enabling and disabling adding
        the debugging behaviors described within @racketvarfont{layer-form} to the source code, while the debugging definitions
        within the layer are still available to other layers.  
        }
  @item{The forms @tt{(@racket[export] @racketvarfont{id} @racketvarfont{id} ...)} and @tt{(import @racketvarfont{layer-id} @racketvarfont{layer-id} ...)} declare
        exports and imports of a layer, where the @racketvarfont{id} is the identifier of an internal
        layer definition and @racketvarfont{layer-id} is some layer identifier.}   
  @item{The internal layer definitions supports code abstraction and reusability.
        The identifier @racketvarfont{debug-src-id} in the form @tt{(@racket[def] @racketvarfont{debug-src-id} @racket[#:src] @racketvarfont{source-expr} @racketvarfont{source-expr} ...)} refers
        to a sequence of source expressions following after the @racket[#:src] keyword, and the
        expression @tt{(@racket[ref] @racketvarfont{debug-src-id})} returns the corresponding source expressions. In a similar way, the
        identifier @racketvarfont{debug-id} in the form @tt{(@racket[def] @racketvarfont{debug-id} @racket[#:debug] @racketvarfont{match-expr} @racketvarfont{match-expr} ...)} is bound
        to a sequence of debugging expressions and the expression @tt{(@racket[ref] @racketvarfont{debug-id})} returns the corresponding
        debugging expressions.} 
  @item{In the form @tt{(@racket[in] @racket[#:file] @racketvarfont{module-path match-expr match-expr} ...)}, the specification for @racketvarfont{module-path} 
        can be three kinds of paths: a relative path, an absolute path, or a library path. For example, the following is
        acceptable specification for @racketvarfont{module-path}.
        @racketblock[
        (code:comment "a relative path")
        (in #:file "src.rkt" ....)
        (code:comment "an absolute path")
        (in #:file (file "/home/xiangqi/test/src.rkt" ....))
        (code:comment "a library path")
        (in #:file test/src ....)
        ]}
  @item{The form @tt{(@racket[with-behavior] @racketvarfont{f template})} defines the behavior of the @racketvarfont{f} function, which is only useful
        when it goes with the tracing @racket[log] function. See @secref["log"] for more information about the usage.}
  @item{The @"@" notation provides a way to distinguish debugging primitives from the primitives in the target language where 
        programmers can mix them in the target program without ambiguity. Currently there are three kinds of debugging 
        primitives: @"@"@racket[function-name], @"@"@racket[ret], and @"@"@racket[,par] where @racket[par] represents any parameter of 
        a function and is matched by the parameter name.
        
        The @"@"@racket[function-name] primitive exposes the run-time function scope to programmers, which is only available to 
        debuggers. For example, the
        form 
        
        [@racket[each-function] [@racket[on-entry] (@racket[log] @racket["function ~a entered"] @"@"@racket[function-name])]]
        
        helps programmers keep track of the control flow of the program without manually adding tracing functions in every function that
        is likely to reach in the run time.
        
        The @"@"@racket[ret] and @"@"@racket[,par] primitives can only be used within @racket[template] in @racket[(with-behavior f template)],
        where @"@"@racket[ret] contains the return value of @racket[f] function call and @"@"@racket[,par] contains the value of the 
        parameter @racket[par]. For example, one possible usage might be
        
        @racket[(with-behavior f "f takes @,x and @,y and returns @ret")].
       }
  @item{The core of the language's power to describe how the source program should exhibit the desirable debugging
        behaviors is captured by @racketvarfont{match-expr}, which involves @emph{where} to pinpoint and @emph{what} to do. 
        
        As the Medic language is intended to be @emph{target-language-independent} that works with most popular programming
        language as much as possible, a minimum set of scope categories is chosen: module scope and function scope. For 
        example, for the above grammar, the third clause of the @racketvarfont{match-expr} non-terminal is within module scope, 
        and the following three clauses are within function scope. Function scope can be function name exact matching or 
        pattern matching. The form @tt{(@racketvarfont{f f} ...)} matches one or more function names enclosed in the parenthesis, and
        @racketvarfont{fun-patten-expr} matches a pattern of function names starting with some common characters, which are components
        of the string @racketvarfont{part-of-target-language-function-name} in @tt{(@racket[with-start] @racketvarfont{part-of-target-language-function-name})}.
        The debugging primitive @racket[each-function] supports referring to every function defined in 
        the module. 
        @margin-note{When the target language is Racket, the @racketvarfont{location-expr} anchor expression in the 
        form @tt{(@racket[at] @racketvarfont{location-expr} ...)} cannot be the internal definition, such as @racket[(define ....)] form inside a 
        function or @racket[(let ....)] local binding form.}
        With clear scope declared for debugging, exact location descriptions are supported by @racketvarfont{border-expr} and 
        @racketvarfont{at-expr}. The goal of @racketvarfont{at-expr} is to facilitate accurately locating the target expression anywhere
        in the source program. The @racketvarfont{location-expr} expression in the form @tt{(@racket[at] @racketvarfont{location-expr} ...)} can be a 
        complete expression in the target program or a part of the expression represented by @racketvarfont{at-pattern-expr} when
        the expression is complicated. To avoid the confusions of multiple matches of @racketvarfont{location-expr} in the target
        program, specification of @racketvarfont{before-expr} and @racketvarfont{after-expr} can be employed to confine the lexical context 
        of the target expression @racketvarfont{location-expr}. If @racketvarfont{border-expr} is within @racketvarfont{at-expr}, the debugging code 
        @tt{@racketvarfont{source-expr source-expr} ...} is inserted before or after the source expression matched by @racketvarfont{at-expr};
        otherwise, it is inserted at the beginning or the end of a function or module. When there are multiple @racketvarfont{match-expr}s
        that contains to-be-inserted debugging code @racket[a], @racket[b], and @racket[c] individually and all requires to be added
        before the same source expression @racket[d], the order of the final modified program will be @racket[c b a d]. On the 
        contrary, when they are all after the expression @racket[d], the order will be @racket[d a b c].
        
        }
]
 
@section{Medic Tracing}

@defmodule[medic/trace]

@defproc*[([(log [datum any/c]) void?]
           [(log [form string?] [v any/c] ...) void?])]{
Adds a log entry in the Log pane. For the latter @racket[log] form, the log entry is a string @racket[form] with @litchar{~a} replaced
by the corresponding value among @racket[v]s.The number of @litchar{~a}s in @racket[form] must be
the same as the number of @racket[v]s.} Examples are as follows:
@codeblock{
(log "Hello World")
(log "function ~a entered" @"@"function-name)
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
Debugging with the Medic debugger consists of three kinds of programs: source programs, Medic programs (by convention
ending with ``-medic.rkt''), and a 
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
  @item{Show the layer of interest.}
  ]
The content of the log entry produced by @racket[(log datum)] varies with the datum type. If there is any context information
about the datum that is available to the debugger such as the name of @racket[datum], it is displayed along with the value 
of @racket[datum]. However, with @racket[(with-behavior f template)] definition in the Medic program, the logging behavior
of @racket[f] function calls is switched to displaying the behavior of the function @racket[f].

Suppose the value of @racket[x] is 3 and we call @racket[(log x)]. Instead of merely printing out the value of @racket[x],
it prints out ``@racket[x] = 3'', which displays the extra context information of the value 3---it is the variable
@racket[x] that we are inspecting. 

All traditional print-like expressions are concerned with displaying values of data, but under some debugging 
circumstances, showing the behavior of data is needed. Consider the following example:

@racketblock[
(define (f x y)
  (+ (sqr x) (sqr y)))
]

When we call @racket[(log (f 3 4))], it produces a tracing log ``(@racket[f] 3 4) = 25'', which reveals no information
about what the @racket[f] function does. To change the behavior of @racket[(log (f 3 4))], we can modify
the Medic program by adding @racket[(with-behavior f "Calling f: sum of @,x squared and @,y squared is @ret")]. The @"@" notation
offers a way to obtain the values of arguments of a function as well as the function returning value. For example, the above
@"@"@racket[,x] gets the value of @racket[x] and @"@"@racket[ret] keeps the returning value of the @racket[f] function call. 
Then the call of @racket[(log (f 3 4))] generates ``Calling f: sum of 3 squared and 4 squared is 25''. The benefits of 
allowing @racket[log] to show the behavior of functions are that programmers have control over writing the descriptions
of functions and changing the description at one place can change all behaviors of related function calls at different
places.

It always happens that the traces become harder to understand with the increase of size, necessitating programmers only
seeing parts of traces of interest. The @emph{layer Viewer} feature of @racket[log] offers a way to focus on relevant traces while
preserving the execution order of traces. 

The following source program traverses a tree to find the path to a desirable node. 
@codeblock{
#lang racket

(define (find-path t name)
  (cond
    [(string? t) (if (equal? t name) '() #f)]
    [else
     (let ([left-p (find-path (cadr t) name)])
       (if left-p 
           (cons (car t) left-p)
           (let ([right-p (find-path (caddr t) name)])
             (if right-p 
                 (cons (car t) right-p)
                 #f))))]))

(find-path '("a" ("b" "1" "2") ("c" "3" "4")) "3")
}
Suppose we want to insert some @racket[log] expressions to see how the tree is traversed.
@codeblock{
#lang medic

(layer left-path
       (in #:file "src.rkt"
           [(at (with-start "(if left-p")) [on-entry (log "left branch: ~a, ~a" (cadr t) left-p)]]))

(layer right-path
       (in #:file "src.rkt"
           [(at (with-start "(if right-p")) [on-entry (log "right branch: ~a, ~a" (caddr t) right-p)]]))
}
We start a debugging session and a trace browser is opened after the evaluation of Medic programs and augmented source programs.
@centered{@image{scribblings/layer1.png}}
What if we just want to see the path of left branches? By clicking on the ``Log Viewer'' button, a Layer Viewer window pops up, 
displaying check boxes of existing layer names. 
@centered{@image{scribblings/layer2.png}}
Select the @racket[left-path] check box, the Log Viewer is updated immediately, highlighting the traces which belong to the 
layer @racket[left-path].
@centered{@image{scribblings/layer3.png}}
@subsection{Tracing Graph}
A tracing graph presents a new means of tracing, allowing programmers to visually see the @emph{spatial} relationship
between trace elements. Text-based and linear traces can print out primitive values and preserve the execution order of programs,
but are limited for values that are reference types or compound data structure, and may exhibit connective relationship. The tracing
graph eases the burden of programmers visualizing the @emph{spatial} relationship in mind or drawing the graph manually on the paper by
adding a lot of text-based tracing functions to print out the relationship, which is @emph{textual} and not @emph{visual} enough. To avoid any overlap of graph nodes and
achieve an aesthetically pleasing visual effect, the tracing graph adopts force-directed algorithms for layout.

Here is one example illustrating the effectiveness of tracing graphs to find a bug in programs that is hard to manifest itself in
text-based traces.

Suppose we have an implementation of the doubly linked list with support for common accessing, inserting, and removing elements
operations. We comment out the line at line number 96 to create a bug. 

@codeblock[#:line-numbers 1]{
#lang racket

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
            ;(set-field! previous temp-next temp-prev)
            (set! size (sub1 size))
            res])]))))
}

Now we want to verify the correctness of the implementation. Traditionally, we would write the following Medic program to debug the source 
implementation. 
@codeblock{
#lang medic

(layer layer1 
       (in #:file "doubly-linked-list.rkt"
           [on-exit
            (define dlist (new doubly-linked-list%))
            ; add ten elements
            (for ([i (reverse (build-list 10 values))]) (send dlist add-at 0 i))
            (for ([i (in-range (send dlist get-size))])
              (log "i=~a, datum=~a" i (send dlist element-at i)))
            
            ; remove five successive elements starting from the fourth element
            (for ([i (in-range 5)]) (send dlist remove 3))
            (for ([i (in-range (send dlist get-size))])
              (log "after removal: i=~a, datum=~a" i (send dlist element-at i)))]))
}

We are presented with a trace browser window containing a Log pane:
@centered{@image{scribblings/log.png}}
It seems like the insertion operation with the list behaves correctly, but there is something wrong with the removal operation---the final list should be the sequence 0, 1, 2, 8, 9 
instead of 0, 1, 2, 4, 5. The tracing logs give us little clue about the cause of the problem, and it requires a 
substantial amount of time to set a breakpoint to step though the program and examine the @racket[previous] and
@racket[next] references of each node. But if we modify the Medic program by trying the tracing graph, we can see
the problem instantly.
@codeblock{
#lang medic
;; disable this layer first
(layer layer1 #:enable #f
       (in #:file "doubly-linked-list.rkt"
           [on-exit
            (define dlist (new doubly-linked-list%))
            ; add ten elements
            (for ([i (reverse (build-list 10 values))]) (send dlist add-at 0 i))
            (for ([i (in-range (send dlist get-size))])
              (log "i=~a, datum=~a" i (send dlist element-at i)))
            
            ; remove five successive elements starting from the fourth element
            (for ([i (in-range 5)]) (send dlist remove 3))
            (for ([i (in-range (send dlist get-size))])
              (log "after removal: i=~a, datum=~a" i (send dlist element-at i)))]))
              
;; add a new layer using graph visualization
(layer layer2
       (in #:file "doubly-linked-list.rkt"
           [on-exit
            (define dlist (new doubly-linked-list%))
            (for ([i (reverse (build-list 10 values))]) (send dlist add-at 0 i))
            (for ([i (in-range 5)]) (send dlist remove 3))
            (for/fold ([temp (get-field head dlist)]) 
              ([i (in-range (sub1 (send dlist get-size)))])
              (define next (get-field next temp))
              ; draw an edge from the current node to its next referencing node with the red arrow color
              (edge temp next "" "Red" (get-field datum temp) (get-field datum next))
              next)
            (for/fold ([temp (get-field next (get-field head dlist))])
              ([i (in-range (sub1 (send dlist get-size)))])
              (define prev (get-field previous temp))
              ; draw an edge from the current node to its previous referencing node with the default gray arrow color
              (edge temp prev "" #f (get-field datum temp) (get-field datum prev))
              (get-field next temp))]))
}
We restart the debugging session and the trace browser is opened where the edges and nodes are visualized in the 
Graph pane. From the graph, we can visually notice that the doubly linked list is broken: a correct list should
have the property that every edge between nodes is bi-directed. The previous reference of node 4 is still 
pointing to the old node 3, which is the fourth node in the list we intend to remove from the list in the first iteration 
of @racket[(send dlist remove 3)] operation. As a result, we can narrow the problem scope down to incorrect previous 
reference updating with the @racket[remove] method, leading us to go back to the relevant code in the @racket[remove] 
implementation and catch the bug of neglecting handling the previous reference of a node which is commented out in the code.

@centered{@image{scribblings/graph.png}}
@subsection{Aggregate View}

@subsection{Timeline View}

@section[#:style '(toc)]{Medic by Example}

This section covers several small examples to help learn the Medic language and use the Medic debugging tool. 
Each example contains a source program and a medic program. Assuming the source program is @tt{src.rkt} that 
is the entry point of the program and the medic
program is @tt{src-medic.rkt}, and the source program, medic program and debugging script are stored in the same
directory, we can start debugging by the following debugging script:
@codeblock{
#lang racket
(require medic/core)
(medic "src-medic.rkt")
(debug "src.rkt")
}

We can also run debugging scripts in the console window of the DrRacket programming environment. To best locate
the files, complete paths of programs---depending on the stored location---should be supplied.

@racketblock[
console prompt: (require medic/core)
console prompt: (medic "/home/xiangqi/medic/demos/src-medic.rkt")
console prompt: (debug "/home/xiangqi/medic/demos/src.rkt")
]

The following are the demos:

@local-table-of-contents[]

@subsection{Demo 1: @tt{border-expr} and @tt{at-expr}}
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
@subsection{Demo 2: @tt{at-expr}}
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
@subsection{Demo 3: multiple functions scope}
Multiple functions involved in the debugging activity.

@bold{@tt{src3.rkt:}}
@codeblock{
#lang racket

(define counter 0)

(define (inc-counter) (set! counter (add1 counter)))

(define (inc x) 
  (inc-counter)
  (+ x 1))

(define (g x)
  (inc x))

(g 4)
}

@bold{@tt{src3-medic.rkt:}}
@codeblock{
#lang medic

(layer layer1 
       (in #:file "src3.rkt"
           ; scope of multiple functions 
           [(g inc) [on-entry (log "function ~a: x = ~a" @"@"function-name x)]]
           ; fun-pattern-expr
           [(with-start "inc") [on-entry (log "function ~a with starting inc function name" @"@"function-name)]]
           ; each-function primitive
           [each-function [on-entry (log "function ~a entered" @"@"function-name)]]))
}

@subsection{Demo 4: @tt{with-behavior}}

@bold{@tt{src4.rkt:}}
@codeblock{
#lang racket

(define (f x y)
  (+ (sqr x) (sqr y)))
}
@bold{@tt{src4-medic.rkt:}}
@codeblock{
#lang medic

(layer layer1 
       (in #:file "src4.rkt"
           (with-behavior f "Calling f: sum of @"@",x squared and @"@",y squared is @"@"ret")
           [on-exit (log (f 3 4))
                    (log (f 4 5))]))
}

@subsection{Demo 5: @tt{def}, @tt{import} and @tt{export}}
@bold{@tt{f.rkt:}}
@codeblock{
#lang racket

(provide f)

(define (f x y)
  (+ (sqr x) (sqr y)))
}
@bold{@tt{src5.rkt:}}
@codeblock{
#lang racket

(require "f.rkt")

(define t 5)

(define (g x)
  (* x (f x t)))

(g 3)
}
@bold{@tt{src5-medic.rkt:}}
@codeblock{
#lang medic

(layer layer1
       (export log-function-entry)
       ; debug-src-id definition
       (def init-defs #:src (define id-count 0)
                            (define (inc-id-count) (set! id-count (add1 id-count))))
       (def inc-id-count #:src (inc-id-count))
       (def display-count #:src (log id-count))
       ; debug-id definition
       (def log-function-entry 
         #:debug 
         [each-function [on-entry (log "function ~a entered" @"@"function-name)]])
       (in #:file "src.rkt"
           [on-entry (ref init-defs)]
           [(at (with-start "(define")) [on-entry (ref inc-id-count)]]
           (ref log-function-entry)
           [on-exit (ref display-count)]))

(layer layer2
       (import layer1)
       (in #:file "f.rkt"
           (ref log-function-entry))
       (in #:file "src.rkt"
           [on-exit (log t)]))
}