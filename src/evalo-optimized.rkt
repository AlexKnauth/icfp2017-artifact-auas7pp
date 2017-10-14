#lang icfp2017-minikanren/racket-scheme-compat

(provide evalo)

(require "mk/mk.rkt")

(include "evalo-optimized.scm")
(declare-racket-wrapper-for "evalo-optimized.scm")
