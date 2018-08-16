(define-library (scheme r5rs)
  (import (scheme base))
  (export quote lambda if set!
	  cond => else case and or
	  let let* letrec
	  begin do
	  quasiquote unquote unquote-splicing
          let-syntax letrec-syntax
	  syntax-rules _ ...
	  define define-syntax
	  ;; # Equivalence predicates
	  eqv? eq? equal?
	  ;; # Numbers
	  number? complex? real? rational? integer?
	  exact? inexact?
	  zero? positive? negative? odd? even?
	  max min * + - / < <= = > >= abs
	  quotient remainder modulo
	  gcd lcm
	  numerator denominator
	  floor ceiling truncate round
	  rationalize
	  expt
	  exact->inexact inexact->exact
	  number->string string->number
	  ;; # Boolean
	  not boolean?
	  ;; # Pairs and lists
	  pair? cons car cdr set-car! set-cdr!
	  caar cadr cdar cddr
	  null? list? list length append reverse list-tail list-ref
	  memq memv member assq assv assoc
	  ;; # Symbols
	  symbol? symbol->string string->symbol
	  ;; # Characters
	  char? char=? char<? char>? char<=? char>=?
	  char->integer integer->char
	  ;; # Strings
	  string? make-string string
	  string-length string-ref string-set!
	  string=?
	  string<? string>? string<=? string>=?
	  substring string-append
	  string->list list->string
	  string-copy string-fill!
	  ;; # Vectors
	  vector? make-vector vector
	  vector-length vector-ref vector-set!
	  vector->list list->vector vector-fill!
	  ;; # Control features
	  procedure? apply
	  map for-each
	  call-with-current-continuation
	  values call-with-values
	  dynamic-wind
	  ;; # Input and output
	  input-port? output-port?
	  current-input-port current-output-port
	  close-input-port close-output-port
	  read-char peek-char eof-object? char-ready?
	  newline write-char)
  (cond-expand
   ((library (scheme file))
    (import (scheme file))
    (export call-with-input-file call-with-output-file
	    with-input-from-file with-output-to-file
	    open-input-file open-output-file))
   (else
    (begin)))
  (cond-expand
   ((library (scheme lazy))
    (import (scheme lazy))
    (export delay force))
   (else
    (begin)))
  (cond-expand
   ((library (scheme inexact))
    (import (scheme inexact))
    (export exp log sin cos tan asin acos atan sqrt))
   (else
    (begin)))
  (cond-expand
   ((library (scheme complex))
    (import (scheme complex))
    (export make-rectangular make-polar real-part imag-part magnitude angle))
   (else
    (begin)))
  (cond-expand
   ((library (scheme cxr))
    (import (scheme cxr))
    (export caaar caadr cadar caddr cdaar cdadr cddar cdddr
            caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
            cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr))
   (else
    (begin)))
  (cond-expand
   ((library (scheme char))
    (import (scheme char))
    (export char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
	    char-alphabetic? char-numeric? char-whitespace? char-upper-case? char-lower-case?
	    char-upcase char-downcase
	    string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?))
   (else
    (begin)))
  (cond-expand
   ((library (scheme eval))
    (import (scheme eval))
    (export eval
	    scheme-report-environment
	    null-environment)
    (begin
      (define (scheme-report-environment version)
	(unless (= version 5)
	  (error "unsupported version" version))
	(environment '(scheme r5rs)))

      (define (null-environment version)
	(unless (= version 5)
	  (error "unsupported version" version))
	(apply environment
	       '(only (scheme base)
		      quote lambda if set!
		      cond => else case and or
		      let let* letrec
		      begin do
		      quasiquote unquote unquote-splicing
		      let-syntax letrec-syntax
		      syntax-rules _ ...
		      define define-syntax)
	       (cond-expand
		((library (scheme lazy))
		 (list '(only (scheme lazy)
			      delay)))
		(else
		 '()))))))
   (else
    (begin)))
  (cond-expand
   ((library (scheme repl))
    (import (scheme repl))
    (export interaction-environment))
   (else
    (begin)))
  (cond-expand
   ((library (scheme load))
    (import (scheme load))
    (export load))
   (else
    (begin)))
  (cond-expand
   ((library (scheme write))
    (import (scheme write))
    (export write display))
   (else
    (begin)))
  (cond-expand
   ((library (scheme read))
    (import (scheme read))
    (export read))
   (else
    (begin))))
