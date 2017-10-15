#lang icfp2017-minikanren/racket-scheme-compat

(require "test-type-infer.rkt"
         "test-interp.rkt"
         "test-quines.rkt"
         "test-arithmetic.rkt")

(include "test-all.scm")
(declare-racket-wrapper-for "test-all.scm")
