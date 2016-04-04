#lang racket/base

(require multimethod
         rackunit)

(define-generic (map _ f))

(struct nothing ())
(struct just (value))

(define-instance ((map just) f j)
  (just (f (just-value j))))

(define-instance ((map nothing) f _)
  (nothing))

(check-equal? (map add1 (just 2)) (just 3))
(check-equal? (map add1 (nothing)) (nothing))
