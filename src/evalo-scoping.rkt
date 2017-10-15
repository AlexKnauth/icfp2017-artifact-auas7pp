#lang icfp2017-minikanren/racket-scheme-compat

(provide eval-lexo eval-dyno)

(require "mk/mk.rkt")

(include "evalo-scoping.scm")
(declare-racket-wrapper-for "evalo-scoping.scm")
