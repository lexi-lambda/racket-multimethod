#lang racket/base

(require racket/function
         racket/struct-info
         (for-syntax racket/base
                     racket/function
                     racket/list
                     racket/provide-transform
                     racket/struct-info
                     racket/syntax
                     syntax/parse
                     "privilege.rkt"))

(provide (rename-out [privileged-struct struct])
         define-generic define-instance)

(begin-for-syntax
  (struct multimethod (arity dispatch-table)
    #:property prop:procedure
    (λ (method stx)
      (syntax-parse stx
        [(method arg ...)
         #'(apply-multimethod method (list arg ...))]
        [method
         #'(λ args (apply-multimethod method args))])))

  (define (assert-privileged-struct! id)
    (unless (id-privileged? id)
      (raise-syntax-error 'define-instance
                          "expected name of struct defined in current module"
                          id))))

(define-syntax privileged-struct
  (syntax-parser
    [(_ name:id fields option ...)
     (mark-id-as-privileged! #'name)
     #'(struct name fields #:transparent option ...)]))

(define-syntax define-generic
  (syntax-parser
    [(_ (method:id arg:id ...+))
     (with-syntax ([arity (length (attribute arg))]
                   [dispatch-table (generate-temporary #'method)])
       (mark-id-as-privileged! #'method)
       #'(begin
           (define dispatch-table (make-hash))
           (define-syntax method (multimethod arity #'dispatch-table))))]))

(define-syntax define-instance
  (syntax-parser
    ; standard (define (proc ...) ...) shorthand
    [(_ ((method type:id ...) . args) body:expr ...+)
     #'(define-instance (method type ...) (λ args body ...))]
    ; full (define proc lambda-expr) notation
    [(_ (method type:id ...) proc:expr)
     (let* ([multimethod (syntax-local-value #'method)]
            [privileged? (id-privileged? #'method)])
       ; don’t check struct privilege if the multimethod is itself privileged
       (unless privileged?
         (map assert-privileged-struct! (attribute type)))
       (with-syntax ([dispatch-table (multimethod-dispatch-table multimethod)]
                     [(struct-type-id ...) (map (compose1 first extract-struct-info syntax-local-value)
                                                (attribute type))])
         #'(let ([struct-types (list struct-type-id ...)])
             (hash-set! dispatch-table struct-types proc))))]))

(define (struct-type-info s)
  (let-values ([(type complete?) (struct-info s)])
    type))

(define-syntax apply-multimethod
  (syntax-parser
    [(_ method args:expr)
     (let ([multimethod (syntax-local-value #'method)])
       (with-syntax ([dispatch-table (multimethod-dispatch-table multimethod)])
         #'(do-apply-multimethod dispatch-table args)))]))

(define (do-apply-multimethod dispatch-table args)
  (apply (hash-ref dispatch-table (map struct-type-info args)) args))
