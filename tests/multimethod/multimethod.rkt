#lang racket/base
(module test racket/base
  (module struct-definitions racket/base
    (provide (struct-out num)
             (struct-out vec))
    (struct num (val) #:transparent)
    (struct vec (val) #:transparent))

  (module generic-definitions racket/base
    (require multimethod
             racket/function
             (submod ".." struct-definitions))

    (provide add)

    (define-generic (add a b))

    (define-instance ((add num num) a b)
      (num (+ (num-val a) (num-val b))))
    (define-instance ((add num vec) n v)
      (vec (map (curry + (num-val n)) (vec-val v))))
    (define-instance ((add vec num) v n)
      (add n v)))

  (module extra-definitions racket/base
    (require multimethod
             (submod ".." generic-definitions))
    (provide (struct-out bool))

    (struct bool (val))
    (define-instance ((add bool bool) a b)
      (bool (or (bool-val a) (bool-val b)))))
  
  (require multimethod
           racket/function
           rackunit
           syntax/macro-testing)

  (require 'struct-definitions
           'generic-definitions
           'extra-definitions)

  (check-equal? (add (num 1) (num 2)) (num 3))
  (check-equal? (add (num 1) (vec '(1 2 3))) (vec '(2 3 4)))
  (check-equal? (add (vec '(1 2 3)) (num 1)) (vec '(2 3 4)))

  (check-equal? (add (bool #f) (bool #f)) (bool #f))
  (check-equal? (add (bool #t) (bool #f)) (bool #t))
  (check-equal? (add (bool #f) (bool #t)) (bool #t))
  (check-equal? (add (bool #t) (bool #t)) (bool #t))

  (check-exn #rx"^define-instance: expected name of struct defined in current module$"
             (thunk (convert-syntax-error (define-instance ((add num bool) n b)
                                            (bool (or (not (= (num-val n) 0)) (bool-val b))))))))
