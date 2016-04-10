#lang scribble/manual

@(require racket/require
          (for-label multimethod
                     (subtract-in racket/base multimethod))
          scribble/eval)

@(module base-forms racket/base
   (require (for-label racket/base)
            scribble/manual)
   (provide base:struct)
   (define base:struct @racket[struct]))

@(require 'base-forms)

@title{Dynamic multiple dispatch}

@defmodule[multimethod]

This library provides syntactic forms for defining and implementing @deftech{multimethods},
dynamically polymorphic functions that support
@hyperlink["https://en.wikipedia.org/wiki/Multiple_dispatch"]{multiple dispatch}. Multimethods are
functions that can have many different implementations depending on the types of arguments they are
invoked on. For example, a generic @tt{add} function might have different implementations for adding
scalars and vectors.

Multimethods provide similar but distinct functionality from @racketmodname[racket/generic], which
permits enhancing implementing structures in more powerful ways, but only supports
@emph{single dispatch}.

@section{Example}

@(interaction
  #:eval ((make-eval-factory '(multimethod)))
  (struct num (val))
  (struct vec (vals))
  
  (define-generic (mul a b))
  
  (define-instance ((mul num num) x y)
    (num (* (num-val x) (num-val y))))
  
  (define-instance ((mul num vec) n v)
    (vec (map (curry * (num-val n)) (vec-vals v))))
  
  (define-instance ((mul vec num) v n)
    (mul n v)))

@section{API Reference}

@defform[#:literals [_]
         (define-generic (name-id param-or-hole ...+))
         #:grammar
         ([param-or-hole param-id _])]{
Defines a new multimethod with the name @racket[name-id]. Each @racket[param-or-hole] corresponds to
a formal parameter to the function. Each argument given a name will be considered for dispatch, but
arguments may be simply passed through by replacing them with @racket[_].}

@defform*[[(define-instance (name-id type-id ...+) proc-expr)
           (define-instance ((name-id type-id ...+) formal-id ...+) body ...+)]]{
Defines a new instance of the multimethod bound by @racket[name-id] for the combination of types
provided, where each @racket[type-id] refers to a
@seclink["structinfo" #:doc '(lib "scribblings/reference/reference.scrbl")]{structure type transformer
binding}.

When using the first form of @racket[define-instance], @racket[proc-expr] should produce a procedure
that will be invoked when an invokation of the multimethod matches the provided types. The second form
is analogous to the usual function definition shorthand, such as the second form of @racket[define].

New multimethod instances cannot be defined on any combination of datatypes—there are rules that
govern which instances are valid. Specifically, a multimethod instance is only valid if @emph{either}
of the following conditions are met:

  @itemlist[
    @item{The multimethod bound by @racket[name-id] was defined in the same module as the instance
          definition.}
    @item{@emph{Any} of the types bound by the @racket[type-id]s were defined in the same module as
          the instance definition.}]

These requirements guarantee that there cannot be two conflicting instances defined in separate
modules, which would cause problems when both loaded at the same time.}

@defform[(struct id fields options)]{
Like @base:struct from @racketmodname[racket/base], but wrapped to cooperate with the instance
validity checking of @racket[define-instance]. Additionally, all structs defined with this form are
@racket[#:transparent]. Otherwise identical to @|base:struct|.}
