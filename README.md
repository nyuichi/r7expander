# TL;DR

```scheme
;;; test.sld
(define-library (foo bar)
  (import (scheme base))
  (begin
    (define x 1)
    (let ((y 2))
      (+ x y))))
```

```shell
$ ./r7expander.scm -l test.sld
(begin
  (define foo.bar:x 1)
  ((lambda (%y.0) (+ foo.bar:x %y.0)) 2))
>
```

# r7expander: a macro expander for R7RS

r7expander is a macro expander for R7RS.
This expander implements an expander for R7RS macros and libraries.
Given a r7rs program or library, it will

1. resolve library import sets and manage export sets,
2. rename local/global variables, and
3. expand all macros.

r7expander is inspired by Al Petrosky's alexpander.
Similarly to his system, r7expander is a simple data-in data-out program.
Some differences are listed below:

- Main part of the program is implemented as reusable R7RS libraries.
- It deals with the library system.
- Technically, it uses syntactic closures as basic blocks of hygienic expansion.

Produced programs will be valid R5RS programs that may contain R5RS primitives

- function calls, variables, constants,
- if, quote, set!, lambda, define (only binary ones), begin,

together with

- parameterize,
- define-record-type, and
- case-lambda.

Internal definitions are not translated into letrec* because outputs can have internal define-record-type, which we don't know how to expand into internal (ordinary) definitions.

# Implementation notes

Non-standard built-in libraries listed below:

- `(r7expander builtin)` exports basic keywords such as `define` and `lambda`.
- `(r7expander native)` exports all native procedures available.