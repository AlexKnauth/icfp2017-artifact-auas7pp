#lang icfp2017-minikanren/racket-scheme-compat

(require "test-check.rkt" "mk.rkt" "arithmetic.rkt")

(include "test-arithmetic.scm")
(declare-racket-wrapper-for "test-arithmetic.scm")
