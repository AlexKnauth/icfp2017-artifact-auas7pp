#lang icfp2017-minikanren/racket-scheme-compat

(require "test-check.rkt" "mk.rkt" "evalo.rkt")

(include "test-quines.scm")
(declare-racket-wrapper-for "test-quines.scm")
