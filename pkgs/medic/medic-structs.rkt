#lang racket

(provide (all-defined-out))

; env structure
; exports, imports: list of identifiers
; import-table: list of pair (layer-id, list-of-exported-ids)
; src-table: map from identifier to source expressions (all expanded, no ref form inside)
; debug-table: map from identifier to debugging matching expressions (may contain ref form, lazy evaluation)
(struct env (exports imports import-table src-table debug-table) #:transparent)

; at-insert structure
; scope: list of function identifier (string) or ‘module
; target: the target expression to be located (string)
; before: the expressions before the target expression (list of string)
; after: the expressions after the target expression (list of string)
; loc: ‘exit or ‘entry
; exprs: expressions to be inserted
(struct at-insert (scope target before after loc exprs) #:transparent)

(struct finer-at-insert ([scope #:mutable] target [posns #:mutable] loc exprs) #:transparent)