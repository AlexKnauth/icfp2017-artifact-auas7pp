#lang icfp2017-minikanren/racket-scheme-compat

(provide evalo)

(require "mk.rkt")

(include "evalo.scm")
(declare-racket-wrapper-for "evalo.scm")
