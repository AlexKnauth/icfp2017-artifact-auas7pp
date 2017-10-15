#lang icfp2017-minikanren/racket-scheme-compat

(provide !-)

(require "mk.rkt")

(include "type-infer.scm")
(declare-racket-wrapper-for "type-infer.scm")
