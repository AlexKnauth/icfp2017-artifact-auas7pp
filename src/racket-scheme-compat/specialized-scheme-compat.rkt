#lang racket/base

;; This file is meant to provide only the amount of racket -> scheme
;; compatibility that this specific implementation of minikanren needs.
;; For instance, it is not meant to provide scheme's mutable pairs, since
;; this implementation of minikanren only uses pairs as immutable.

(provide (all-from-out
          racket/base
          rnrs/control-6
          rnrs/records/syntactic-6
          rnrs/io/simple-6
          rnrs/io/ports-6
          rnrs/arithmetic/fixnums-6)
         find
         for-all
         exists
         filter
         partition
         fold-left
         fold-right
         remp
         memp
         cons*
         list-sort
         fx=
         fxsll
         fxsra
         gensym?
         include
         declare-racket-wrapper-for
         load
         )

(require racket/require
         (subtract-in racket/base
                      rnrs/control-6
                      rnrs/io/simple-6)
         rnrs/control-6
         rnrs/records/syntactic-6
         rnrs/io/simple-6
         rnrs/io/ports-6
         rnrs/arithmetic/fixnums-6
         (only-in racket/list remf partition))

;; List Utilities
(define find findf)
(define for-all andmap)
(define exists ormap)
(define fold-left foldl)
(define fold-right foldr)
(define remp remf)
(define memp memf)
(define cons* list*)

;; Sorting
(define (list-sort <? lst)
  (sort lst <?))

;; Fixnum Shorthand Operations
(define fx= fx=?)
(define fxsll fxarithmetic-shift-left)
(define fxsra fxarithmetic-shift-right)

;; Symbols
(define (gensym? x)
  (and (symbol? x) (not (symbol-interned? x))))

;; ------------------------------------------------------------------------

;; Load and Include

(require syntax/modresolve
         (only-in racket/include [include include])
         (for-syntax racket/base
                     syntax/path-spec))

(define current-included-paths (make-parameter '()))

;; add-included-path : ModulePath -> Void
(define (add-included-path! path)
  (current-included-paths (cons path (current-included-paths))))

(define-syntax declare-racket-wrapper-for
  (lambda (stx)
    (syntax-case stx ()
      [(_ path)
       #`(add-included-path! '#,(resolve-path-spec #'path stx stx))])))

;; load : ModulePath -> Void
;; Checks that it doesn't need to do the load. If you see an error message
;; from this function, then that means you haven't required or included the
;; file yet. To fix this, in the wrapper file you should require the wrapper
;; file of the given path.
(define (load mp)
  (define mp* (resolve-module-path mp (current-load-relative-directory)))
  (if (member mp* (current-included-paths))
      (void)
      (error 'load
             "require the racket wrapper file for ~v"
             mp)))

;; ------------------------------------------------------------------------

