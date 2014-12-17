#lang scribble/doc

@(require scribble/manual
          redex/pict
          scriblib/figure
          (for-label (except-in racket log export import remove)
                     racket/contract/base
                     medic/trace))

@title{A Metaprogramming Language}
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