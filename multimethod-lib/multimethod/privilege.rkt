#lang racket/base

(require syntax/id-set)

(provide mark-id-as-privileged!
         id-privileged?)

(define privileged-ids (mutable-free-id-set))

(define (mark-id-as-privileged! id)
  (free-id-set-add! privileged-ids id))

(define (id-privileged? id)
  (free-id-set-member? privileged-ids id))
