#lang icfp2017-minikanren/racket-scheme-compat

(provide (all-defined-out))

(require "mk.rkt")

(include "arithmetic.scm")
(declare-racket-wrapper-for "arithmetic.scm")
