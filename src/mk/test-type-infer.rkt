#lang icfp2017-minikanren/racket-scheme-compat

(require "test-check.rkt" "mk.rkt" "type-infer.rkt")

(include "test-type-infer.scm")
(declare-racket-wrapper-for "test-type-infer.scm")
