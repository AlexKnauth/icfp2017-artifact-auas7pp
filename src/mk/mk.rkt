#lang icfp2017-minikanren/racket-scheme-compat

(provide run run* fresh == =/=
         conde conde$ conde$-dfs conde-weighted conde1 conde1$
         lambdag@
         walk
         state-S state-depth state-depth-set state-deferred-defer
         var?
         let/vars
         project0
         numbero symbolo
         unit
         allow-incomplete-search?
         absento)

(include "mk.scm")
(declare-racket-wrapper-for "mk.scm")
