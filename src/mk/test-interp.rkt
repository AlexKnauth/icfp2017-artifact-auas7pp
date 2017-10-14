#lang icfp2017-minikanren/racket-scheme-compat

(require "test-check.rkt" "mk.rkt")

(include "test-interp.scm")
(declare-racket-wrapper-for "test-interp.scm")
