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
    #:transparent
    #:property prop:procedure
    (λ (method stx)
      (syntax-parse stx
        [(method arg ...)
         #'(apply-multimethod method (list arg ...))]
        [method
         #'(λ args (apply-multimethod method args))])))
  
  ; each multimethod has a total arity and a set of indicies for which dispatch is actually performed
  ; for example, consider the definition of “map” — it has a total arity of 2, but dispatch is only
  ; performed on the second argument
  (struct dispatch-arity (total relevant-indicies) #:transparent)
  
  (define-splicing-syntax-class multimethod-arity-spec
    #:attributes [dispatch-arity-expr]
    [pattern (~seq arg:id ...)
             #:attr dispatch-arity-expr
             #`(dispatch-arity #,(length (attribute arg))
                               '#,(for/list ([(id n) (in-indexed (attribute arg))]
                                             #:unless (free-identifier=? id #'_))
                                    n))])

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
    [(_ (method:id arity-spec:multimethod-arity-spec))
     (with-syntax ([dispatch-table (generate-temporary #'method)])
       (mark-id-as-privileged! #'method)
       #'(begin
           (define dispatch-table (make-hash))
           (define-syntax method (multimethod arity-spec.dispatch-arity-expr #'dispatch-table))))]))

(define-syntax define-instance
  (syntax-parser
    ; standard (define (proc ...) ...) shorthand
    [(_ ((method type:id ...+) . args) body:expr ...+)
     #'(define-instance (method type ...) (λ args body ...))]
    ; full (define proc lambda-expr) notation
    [(_ (method type:id ...+) proc:expr)
     (let* ([multimethod (syntax-local-value #'method)]
            [privileged? (id-privileged? #'method)])
       ; don’t check struct privilege if the multimethod is itself privileged
       (unless (or privileged? (ormap id-privileged? (attribute type)))
         (assert-privileged-struct! (first (attribute type))))
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
       (with-syntax ([dispatch-table (multimethod-dispatch-table multimethod)]
                     [relevant-indicies (dispatch-arity-relevant-indicies
                                         (multimethod-arity multimethod))])
         #'(do-apply-multimethod dispatch-table (filter-indicies 'relevant-indicies) args)))]))

; Given a list of indicies and a list, returns a list with only the elements at the specified
; indicies. Used to get the args needed for dispatch from the arity’s relevant-indicies.
; (listof exact-nonnegative-integer?) -> list? -> list?
(define ((filter-indicies indicies) lst)
  (for/list ([(x i) (in-indexed lst)]
             #:when (member i indicies))
    x))

(define (do-apply-multimethod dispatch-table map-args-to-dispatch args)
  (apply (hash-ref dispatch-table (map struct-type-info (map-args-to-dispatch args))) args))

(begin-for-syntax
  (module+ test
    (require rackunit
             rackunit/spec)

    (describe ":multimethod-arity-spec"
      (it "parses syntax to dispatch-arity structs"
        (check-equal? (syntax->datum
                       (syntax-parse #'(a b c d)
                         [(arity-spec:multimethod-arity-spec)
                          (attribute arity-spec.dispatch-arity-expr)]))
                      '(dispatch-arity 4 '(0 1 2 3)))
        
        (check-equal? (syntax->datum
                       (syntax-parse #'(_ f _ _)
                         [(arity-spec:multimethod-arity-spec)
                          (attribute arity-spec.dispatch-arity-expr)]))
                      '(dispatch-arity 4 '(1)))
        
        (check-equal? (syntax->datum
                       (syntax-parse #'(_ a _ b _)
                         [(arity-spec:multimethod-arity-spec)
                          (attribute arity-spec.dispatch-arity-expr)]))
                      '(dispatch-arity 5 '(1 3)))))))
