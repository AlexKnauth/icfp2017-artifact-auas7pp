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

;; Examples:

;; file a.scm, no loads.
;; file a.rkt, the racket wrapper:
;;   #lang icfp2017-minikanren/racket-scheme-compat
;;   (provide a-public-id ...)
;;   (include "a.scm")
;;   (declare-racket-wrapper-for "a.scm")

;; file b.scm:
;;   (load "a.scm")
;; file b.rkt:
;;   #lang icfp2017-minikanren/racket-scheme-compat
;;   (provide b-public-id ...)
;;   (require "a.rkt")
;;   (include "b.scm")
;;   (declare-racket-wrapper-for "b.scm")
;; file c.scm:
;;   (load "a.scm")
;;   (load "b.scm")
;; file c.rkt:
;;   #lang icfp2017-minikanren/racket-scheme-compat
;;   (provide c-public-id ...)
;;   (require "a.rkt" "b.rkt")
;;   (include "c.scm")
;;   (declare-racket-wrapper-for "c.scm")

;; End Example.

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
;; file yet. To fix this, in the wrapper file you should require the
;; wrapper file of the given path.
;; NOTE:
;;   The wrapper file you require should declare itself to be a wrapper
;;   file using declare-racket-wrapper-for.
(define (load mp)
  (define mp* (resolve-module-path mp (current-load-relative-directory)))
  (if (member mp* (current-included-paths))
      (void)
      (error 'load
             "require the racket wrapper file for ~v"
             mp)))

;; ------------------------------------------------------------------------

