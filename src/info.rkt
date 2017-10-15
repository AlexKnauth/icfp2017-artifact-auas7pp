#lang info

;; ---------------------------------------------------------

;; Package Info

(define collection "icfp2017-minikanren")

(define deps
  '("base"
    "r6rs-lib"
    ))

;; ---------------------------------------------------------

;; Collection Info

(define compile-omit-paths
  '("all-challenges.scm"
    "challenge-1.scm"
    "challenge-2-extra-slow.scm"
    "challenge-2.scm"
    "challenge-3.scm"
    "challenge-4.scm"
    "challenge-5.scm"
    "challenge-6.scm"
    "challenge-7.scm"
    "evalo-optimized.scm"
    "evalo-scoping.scm"
    "evalo-small.scm"
    "evalo-standard.scm"
    "intro-examples.scm"
    "love-in-99k-ways.scm"
    "mk/arithmetic.scm"
    "mk/evalo.scm"
    "mk/mk.scm"
    "mk/test-all.scm"
    "mk/test-arithmetic.scm"
    "mk/test-check.scm"
    "mk/test-interp.scm"
    "mk/test-quines.scm"
    "mk/test-type-infer.scm"
    "mk/type-infer.scm"
    ))

