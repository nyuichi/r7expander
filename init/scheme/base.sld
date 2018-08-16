(define-library (scheme base)
  (export
   ;; 4.1.2. Literal expressions
   quote
   ;; 4.1.4. Procedures
   lambda
   ;; 4.1.5. Conditionals
   if
   ;; 4.1.6. Assignments
   set!
   ;; 4.1.7. Inclusion
   include
   ;; 4.2.1. Conditionals
   cond else => case and or when unless cond-expand library
   ;; 4.2.2. Binding constructs
   let let* letrec letrec* let-values let*-values
   ;; 4.2.3. Sequencing
   begin
   ;; 4.2.4. Iteration
   do
   ;; 4.2.6. Dynamic bindings
   make-parameter parameterize
   ;; 4.2.7. Exception handling
   guard
   ;; 4.2.8. Quasiquotation
   quasiquote unquote unquote-splicing
   ;; 4.3.1. Binding constructs for syntactic keywords
   let-syntax letrec-syntax
   ;; 4.3.2 Pattern language
   syntax-rules _ ...
   ;; 4.3.3. Signaling errors in macro transformers
   syntax-error
   ;; 5.3. Variable definitions
   define
   ;; 5.3.3. Multiple-value definitions
   define-values
   ;; 5.4. Syntax definitions
   define-syntax
   ;; 5.5 Record-type definitions
   define-record-type
   ;; 6.1. Equivalence predicates
   eq? eqv? equal?
   ;; 6.2. Numbers
   number? complex? real? rational? integer?
   exact? inexact? exact-integer?
   exact inexact
   = < > <= >=
   zero? positive? negative? odd? even?
   min max + - * / abs
   floor-quotient floor-remainder floor/
   truncate-quotient truncate-remainder truncate/
   (rename truncate-quotient quotient)
   (rename truncate-remainder remainder)
   (rename floor-remainder modulo)
   gcd lcm
   numerator denominator
   floor ceiling truncate round
   rationalize
   exact-integer-sqrt square expt
   number->string string->number
   ;; 6.3. Booleans
   boolean? boolean=? not
   ;; 6.4 Pairs and lists
   pair? cons car cdr set-car! set-cdr!
   caar cadr cdar cddr
   null? list? make-list list
   length append reverse list-tail
   list-ref list-set!
   list-copy
   memq memv member
   assq assv assoc
   ;; 6.5. Symbols
   symbol? symbol=? symbol->string string->symbol
   ;; 6.6. Characters
   char? char->integer integer->char
   char=? char<? char>? char<=? char>=?
   ;; 6.7. Strings
   string? string make-string
   string-length string-ref string-set!
   string=? string<? string>? string<=? string>=?
   (rename string-copy substring)
   string-append
   string->list list->string
   string-copy string-copy! string-fill!
   ;; 6.8. Vectors
   vector? vector make-vector
   vector-length vector-ref vector-set!
   list->vector vector->list
   string->vector vector->string
   vector-copy vector-copy! vector-append vector-fill!
   ;; 6.9. Bytevectors
   bytevector? make-bytevector bytevector
   bytevector-length bytevector-u8-ref bytevector-u8-set!
   bytevector-copy bytevector-copy! bytevector-append
   utf8->string string->utf8
   ;; 6.10. Control features
   procedure? apply
   map for-each
   string-map string-for-each
   vector-map vector-for-each
   call-with-current-continuation
   (rename call-with-current-continuation call/cc)
   values call-with-values
   dynamic-wind
   ;; 6.11. Exceptions
   with-exception-handler
   raise raise-continuable error
   error-object? error-object-message error-object-irritants
   read-error? file-error?
   ;; 6.13. Input and output
   current-input-port current-output-port current-error-port
   call-with-port
   port? input-port? output-port? textual-port? binary-port?
   input-port-open? output-port-open?
   close-port close-input-port close-output-port
   open-input-string open-output-string get-output-string
   open-input-bytevector open-output-bytevector get-output-bytevector
   eof-object? eof-object
   read-char peek-char char-ready? read-line read-string
   read-u8 peek-u8 u8-ready? read-bytevector read-bytevector!
   newline write-char write-string write-u8 write-bytevector
   flush-output-port)

  (import (rename
	   (only (r7expander builtin)
		 lambda define quote if set! begin
		 let-syntax letrec-syntax define-syntax syntax-error
		 syntax-rules _ ...
		 include if-expand
		 define-record-type parameterize)
	   (define define*)))

  (import (only (r7expander native)
		;; 4.2.6. Dynamic bindings
		make-parameter
		;; 6.1. Equivalence predicates
		eq? eqv? equal?
		;; 6.2. Numbers
		number? complex? real? rational? integer?
		exact? inexact? exact-integer?
		exact inexact
		= < > <= >=
		zero? positive? negative? odd? even?
		min max + - * / abs
		floor-quotient floor-remainder floor/
		truncate-quotient truncate-remainder truncate/
		gcd lcm
		numerator denominator
		floor ceiling truncate round
		rationalize
		exact-integer-sqrt square expt
		number->string string->number
		;; 6.3. Booleans
		boolean? boolean=? not
		;; 6.4 Pairs and lists
		pair? cons car cdr set-car! set-cdr!
		caar cadr cdar cddr
		null? list? make-list list
		length append reverse list-tail
		list-ref list-set!
		list-copy
		memq memv member
		assq assv assoc
		;; 6.5. Symbols
		symbol? symbol=? symbol->string string->symbol
		;; 6.6. Characters
		char? char->integer integer->char
		char=? char<? char>? char<=? char>=?
		;; 6.7. Strings
		string? string make-string
		string-length string-ref string-set!
		string=? string<? string>? string<=? string>=?
		string-append
		string->list list->string
		string-copy string-copy! string-fill!
		;; 6.8. Vectors
		vector? vector make-vector
		vector-length vector-ref vector-set!
		list->vector vector->list
		string->vector vector->string
		vector-copy vector-copy! vector-append vector-fill!
		;; 6.9. Bytevectors
		bytevector? make-bytevector bytevector
		bytevector-length bytevector-u8-ref bytevector-u8-set!
		bytevector-copy bytevector-copy! bytevector-append
		utf8->string string->utf8
		;; 6.10. Control features
		procedure? apply
		map for-each
		string-map string-for-each
		vector-map vector-for-each
		call-with-current-continuation
		values call-with-values
		dynamic-wind
		;; 6.11. Exceptions
		with-exception-handler
		raise raise-continuable error
		error-object? error-object-message error-object-irritants
		read-error? file-error?
		;; 6.13. Input and output
		current-input-port current-output-port current-error-port
		call-with-port
		port? input-port? output-port? textual-port? binary-port?
		input-port-open? output-port-open?
		close-port close-input-port close-output-port
		open-input-string open-output-string get-output-string
		open-input-bytevector open-output-bytevector get-output-bytevector
		eof-object? eof-object
		read-char peek-char char-ready? read-line read-string
		read-u8 peek-u8 u8-ready? read-bytevector read-bytevector!
		newline write-char write-string write-u8 write-bytevector
		flush-output-port))
  (begin
    (define-syntax define
      (syntax-rules ()
	((define (identifier . formals) . body)
	 (define identifier
	   (lambda formals . body)))
	((define identifier expr)
	 (define* identifier expr))))

    (define-syntax cond
      (syntax-rules (else =>)
	((cond)
	 (if #f #f))
	((cond (else expr ...))
	 (begin expr ...))
	((cond (test => proc) clause ...)
	 (let ((tmp test))
	   (if tmp
	       (proc tmp)
	       (cond clause ...))))
	((cond (test) clause ...)
	 (or test
	     (cond clause ...)))
	((cond (test expr ...) clause ...)
	 (if test
	     (begin expr ...)
	     (cond clause ...)))))

    (define-syntax case
      (syntax-rules ()
	((case key0 clause0 ...)
	 (letrec-syntax
	     ((case-aux
	       (syntax-rules ::: (else =>)
		 ((_ key)
		  (if #f #f))
		 ((_ key (else expr :::))
		  (begin expr :::))
		 ((_ key (else => proc))
		  (proc key))
		 ((_ key ((atoms :::) => proc) clause :::)
		  (if (memv key '(atoms :::))
		      (proc key)
		      (case-aux key clause :::)))
		 ((_ key ((atoms :::) expr :::) clause :::)
		  (if (memv key '(atoms :::))
		      (begin expr :::)
		      (case-aux key clause :::))))))
	   (let ((tmp key0))
	     (case-aux tmp clause0 ...))))))

    (define-syntax and
      (syntax-rules ()
	((and) #t)
	((and form) form)
	((and form rest ...)
	 (if form
	     (and rest ...)
	     #f))))

    (define-syntax or
      (syntax-rules ()
	((or) #f)
	((or form) form)
	((or form rest ...)
	 (let ((tmp form))
	   (if tmp
	       tmp
	       (or rest ...))))))

    (define-syntax when
      (syntax-rules ()
	((when test expr ...)
	 (if test
	     (begin expr ...)))))

    (define-syntax unless
      (syntax-rules ()
	((unless test expr ...)
	 (when (not test) expr ...))))

    (define-syntax cond-expand		; follows R7RS semantics
      (syntax-rules (else library and or not)
	((cond-expand)
	 (if #f #f))
	((cond-expand (else expr ...))
	 (begin expr ...))
	((cond-expand ((and) expr ...) clause ...)
	 (begin expr ...))
	((cond-expand ((and test1 test2 ...) expr ...) clause ...)
	 (cond-expand
	  (test1
	   (cond-expand
	    ((and test2 ...)
	     expr ...)
	    clause ...))
	  clause ...))
	((cond-expand ((or) expr ...) clause ...)
	 (cond-expand
	  clause ...))
	((cond-expand ((or test1 test2 ...) expr ...) clause ...)
	 (cond-expand
	  (test1 expr ...)
	  ((or test2 ...) expr ...)
	  clause ...))
	((cond-expand ((not test) expr ...) clause ...)
	 (cond-expand
	  (test
	   (cond-expand clause ...))
	  (else
	   expr ...)))
	((cond-expand ((library spec) expr ...) clause ...)
	 (if-expand (library spec)
		    (begin expr ...)
		    (cond-expand clause ...)))
	((cond-expand (feature expr ...) clause ...)
	 (if-expand feature
		    (begin expr ...)
		    (cond-expand clause ...)))))

    (define-syntax let
      (syntax-rules ()
	((let ((var init) ...) body ...)
	 ((lambda (var ...)
	    body ...)
	  init ...))
	((let loop ((var init) ...) body ...)
	 (letrec ((loop (lambda (var ...)
			  body ...)))
	   (loop init ...)))))

    (define-syntax let*
      (syntax-rules ()
	((let* () body ...)
	 (let () body ...))
	((let* ((var init) rest ...) body ...)
	 (let ((var init))
	   (let* (rest ...) body ...)))))

    (define-syntax letrec
      (syntax-rules ()
	((letrec ((var0 init0) ...) body0 ...)
	 (letrec-syntax
	     ((letrec-aux
	       (syntax-rules ::: ()
		 ((_ () (tmp :::) ((var init) :::) . body)
		  (let ((var (if #f #f)) :::)
		    (let ((tmp init) :::)
		      (set! var tmp)
		      :::
		      (let () . body))))
		 ((_ (var vars :::) tmps bindings . body)
		  (letrec-aux (vars :::) (newtmp . tmps) bindings . body)))))
	   (letrec-aux (var0 ...) () ((var0 init0) ...) body0 ...)))))

    (define-syntax letrec*
      (syntax-rules ()
	((letrec* ((var init) ...) body ...)
	 (let ((var (if #f #f)) ...)
	   (set! var init)
	   ...
	   (let ()
	     body ...)))))

    (define-syntax let-values
      (syntax-rules ()
	((let-values ((formals0 init0) ...) body0 ...)
	 (letrec-syntax
	     ((lv-aux
	       (syntax-rules ::: ()
		 ((_ () () bindings . body)
		  (let bindings . body))
		 ((_ new-formals ((() init) . rest) bindings . body)
		  (call-with-values (lambda () init)
		    (lambda new-formals
		      (lv-aux () rest bindings . body))))
		 ((_ (tmp :::) (((x . y) init) . rest) bindings . body)
		  (lv-aux (tmp ::: new-tmp) ((y init) . rest) ((x new-tmp) . bindings) . body))
		 ((_ (tmp :::) ((x init) . rest) bindings . body)
		  (lv-aux (tmp ::: . new-tmp) ((() init) . rest) ((x new-tmp) . bindings) . body)))))
	   (lv-aux () ((formals0 init0) ...) () body0 ...)))))

    (define-syntax let*-values
      (syntax-rules ()
	((let*-values () body ...)
	 (let () body ...))
	((let*-values ((formals init) rest ...) body ...)
	 (let-values ((formals init))
	   (let*-values (rest ...) body ...)))))

    (define-syntax do
      (syntax-rules ()
	((do ((var init step ...) ...)
	     (test epilogue ...)
	   form ...)
	 (let loop ((var init) ...)
	   (if test
	       (begin (if #f #f) epilogue ...)
	       (begin form ... (loop (begin var step ...) ...)))))))

    (define-syntax guard
      (syntax-rules ()
	((guard (var0 clause0 ...) body0 ...)
	 (letrec-syntax
	     ((guard-aux
	       (syntax-rules ::: (else =>)
		 ((_ reraise)
		  reraise)
		 ((_ reraise (else expr :::))
		  (begin expr :::))
		 ((_ reraise (test => proc) clause :::)
		  (let ((tmp test))
		    (if tmp
			(proc tmp)
			(guard-aux reraise clause :::))))
		 ((_ reraise (test) clause :::)
		  (or test
		      (guard-aux reraise clause :::)))
		 ((_ reraise (test expr :::) clause :::)
		  (if test
		      (begin expr :::)
		      (guard-aux reraise clause :::))))))
	   ((call/cc
	     (lambda (guard-k)
	       (with-exception-handler
		(lambda (condition)
		  ((call/cc
		    (lambda (handler-k)
		      (guard-k
		       (lambda ()
			 (let ((var0 condition))
			   (guard-aux (handler-k
				       (lambda ()
					 (raise-continuable condition)))
				      clause0 ...))))))))
		(lambda ()
		  (call-with-values (lambda () body0 ...)
		    (lambda args
		      (guard-k (lambda ()
				 (apply values args))))))))))))))

    (define-syntax quasiquote	; taken from EIOD
      (syntax-rules (unquote unquote-splicing quasiquote)
	((quasiquote (quasiquote x) . d)
	 (list 'quasiquote (quasiquote x d)))
	((quasiquote (unquote x))
	 x)
	((quasiquote (unquote x) d)
	 (list 'unquote (quasiquote x . d)))
	((quasiquote ((unquote-splicing x) . y))
	 (append x (quasiquote y)))
	((quasiquote (unquote-splicing x) d)
	 (list 'unquote-splicing (quasiquote x . d)))
	((quasiquote (x . y) . d)
	 (cons (quasiquote x . d) (quasiquote y . d)))
	;; ((quasiquote #(x ...) . d)
	;;  (vector (quasiquote x . d) ...))
	((quasiquote x . d)
	 'x)))
    
    (define-syntax define-values
      (syntax-rules ()
	((define-values formals init)
	 (define-values-aux formals () () init))))

    (define-syntax define-values-aux ; use define-syntax because letrec-syntax creates an environment
      (syntax-rules ()
	((_ () new-formals ((var tmp) ...) init)
	 (begin
	   (define var (if #f #f))
	   ...
	   (define dummy
	     (call-with-values (lambda () init)
	       (lambda new-formals
		 (set! var tmp)
		 ...)))))
	((_ (x . y) (tmp ...) bindings init)
	 (define-values-aux y (tmp ... new-tmp) ((x new-tmp) . bindings) init))
	((_ x (tmp ...) bindings init)
	 (define-values-aux () (tmp ... . new-tmp) ((x new-tmp) . bindings) init))))))
