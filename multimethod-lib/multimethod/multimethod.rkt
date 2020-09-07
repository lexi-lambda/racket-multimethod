#lang racket/base

(require racket/contract/base
         racket/function
         racket/struct-info
         (for-syntax racket/base
                     racket/function
                     racket/list
                     racket/provide-transform
                     racket/struct-info
                     racket/syntax
                     syntax/parse
                     syntax/transformer
                     "privilege.rkt"))

(provide (rename-out [privileged-struct struct])
         (contract-out (rename multimethod-descriptor? multimethod? (-> any/c boolean?)))
         define-generic define-instance)

; runtime representation of a multimethod
(struct multimethod-descriptor (name relevant-indices dispatch-table)
  #:methods gen:custom-write
  [(define (write-proc method port mode)
     (fprintf port "#<multimethod:~a>" (multimethod-descriptor-name method)))]
  #:property prop:procedure
  (lambda (method . args)
    (apply-multimethod (multimethod-descriptor-name method)
                       (multimethod-descriptor-relevant-indices method)
                       (multimethod-descriptor-dispatch-table method)
                       args)))

(begin-for-syntax
  ; compile-time representation of a multimethod binding
  (struct multimethod-binding (arity descriptor)
    #:property prop:procedure
    (lambda (method stx)
      (let ([descriptor (multimethod-binding-descriptor method)])
        ((make-variable-like-transformer descriptor) stx))))

  ; each multimethod has a total arity and a set of indices for which dispatch is actually performed
  ; for example, consider the definition of “map” — it has a total arity of 2, but dispatch is only
  ; performed on the second argument
  (struct dispatch-arity (total relevant-indices) #:transparent)

  ; handles parsing multimethod arg lists into expressions that produce dispatch-arity structs
  (define-splicing-syntax-class multimethod-arity-spec
    #:attributes [dispatch-arity]
    [pattern (~seq arg:id ...+)
             #:attr dispatch-arity
             (dispatch-arity (length (attribute arg))
                             (for/list ([(id n) (in-indexed (attribute arg))]
                                        #:unless (free-identifier=? id #'_))
                               n))
             #:fail-when (empty? (dispatch-arity-relevant-indices (attribute dispatch-arity)))
             "expected at least one dispatch parameter"])

  ; wrapper around syntax-local-value with better error messages
  (define ((assert-syntax-local-value name) id-stx)
    (syntax-local-value id-stx (lambda () (raise-syntax-error name "unbound identifier" id-stx)))))

; replacement for the struct form that associates privilege information
(define-syntax privileged-struct
  (syntax-parser
    [(_ name:id fields option ...)
     (mark-id-as-privileged! #'name)
     #'(struct name fields #:transparent option ...)]))

(define-syntax define-generic
  (syntax-parser
    [(_ (method:id arity-spec:multimethod-arity-spec))
     (let* ([arity (attribute arity-spec.dispatch-arity)]
            [relevant-indices (dispatch-arity-relevant-indices arity)])
       (with-syntax ([descriptor (generate-temporary #'method)]
                     [arity arity]
                     [relevant-indices relevant-indices])
         (mark-id-as-privileged! #'method)
         #'(begin
             (define descriptor (multimethod-descriptor 'method 'relevant-indices (make-hash)))
             (define-syntax method (multimethod-binding arity #'descriptor)))))]))

(define-syntax define-instance
  (syntax-parser
    ; standard (define (proc ...) ...) shorthand
    [(_ ((~and signature (method type:id ...+)) . args) body:expr ...+)
     #'(define-instance signature (lambda args body ...))]
    ; full (define proc lambda-expr) notation
    [(_ (~and signature (method type:id ...+)) proc:expr)
     (let* ([multimethod ((assert-syntax-local-value 'define-instance) #'method)]
            [privileged? (id-privileged? #'method)])
       (unless (multimethod-binding? multimethod)
         (raise-syntax-error 'define-instance
                             "expected multimethod binding"
                             #'method))
       ; don’t check struct privilege if the multimethod is itself privileged
       (unless (or privileged? (ormap id-privileged? (attribute type)))
         (raise-syntax-error 'define-instance
                             "expected name of multimethod or struct defined in current module"
                             #'signature))
       (with-syntax ([descriptor (multimethod-binding-descriptor multimethod)]
                     [(struct-type-id ...) (map (compose1 first
                                                          extract-struct-info
                                                          (assert-syntax-local-value 'define-instance))
                                                (attribute type))])
         #'(let ([struct-types (list struct-type-id ...)])
             (hash-set! (multimethod-descriptor-dispatch-table descriptor) struct-types proc))))]))

; wrapper around struct-info that throws away the second value
(define (struct-type-info s)
  (let-values ([(type complete?) (struct-info s)])
    type))

; runtime implementation of multimethod dispatch and invocation
(define (apply-multimethod name relevant-indices dispatch-table args)
  (let ([dispatch-args (for/list ([(x i) (in-indexed args)]
                                  #:when (member i relevant-indices))
                         (or (struct-type-info x)
                             (apply raise-argument-error name "transparent struct" i args)))])
    (apply (hash-ref dispatch-table dispatch-args (lambda () (raise-user-error name "no multimethod instance found for ~a" (map object-name dispatch-args)))) args)))

(begin-for-syntax
  (module+ test
    (require rackunit
             rackunit/spec)

    (describe ":multimethod-arity-spec"
      (it "parses syntax to dispatch-arity structs"
        (check-equal? (syntax-parse #'(a b c d)
                        [(arity-spec:multimethod-arity-spec)
                         (attribute arity-spec.dispatch-arity)])
                      (dispatch-arity 4 '(0 1 2 3)))

        (check-equal? (syntax-parse #'(_ f _ _)
                        [(arity-spec:multimethod-arity-spec)
                         (attribute arity-spec.dispatch-arity)])
                      (dispatch-arity 4 '(1)))

        (check-equal? (syntax-parse #'(_ a _ b _)
                        [(arity-spec:multimethod-arity-spec)
                         (attribute arity-spec.dispatch-arity)])
                      (dispatch-arity 5 '(1 3)))

        (check-exn exn:fail:syntax?
                   (lambda ()
                     (syntax-parse #'(_ _)
                       [(arity-spec:multimethod-arity-spec) #f])))))))
