#lang info

(define collection 'multi)

(define deps
  '("base"
    "multimethod-lib"
    "multimethod-doc"))
(define build-deps
  '())

(define implies
  '("multimethod-lib"
    "multimethod-doc"))
