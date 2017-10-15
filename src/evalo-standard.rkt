#lang icfp2017-minikanren/racket-scheme-compat

(provide evalo)

(require "mk/mk.rkt")

(include "evalo-standard.scm")
(declare-racket-wrapper-for "evalo-standard.scm")
