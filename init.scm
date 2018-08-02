(define (install-keyword! keyword transformer)
  (let ((env (current-toplevel-environment)))
    (let ((expander (make-expander transformer env)))
      (install-expander! keyword expander env))))

(define (import-native keyword)
  (let ((env (current-toplevel-environment)))
    (install-toplevel-binding! keyword keyword env)))

(make-library '(scheme base))
(with-library '(scheme base)
  (lambda ()
    (for-each library-export
	      '(;; 4.1.2. Literal expressions
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
		cond else => case and or when unless cond-expand
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
		flush-output-port))

    (for-each import-native
	      '(;; 4.2.6. Dynamic bindings
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

    (install-keyword! 'lambda
      (lambda (form env)
	(unless (>= (length form) 3)
	  (error "malformed lambda" form))
	(let ((formals (cadr form))
	      (body (cddr form)))
	  (let ((formal-list
		 (let loop ((formals formals) (acc '()))
		   (cond ((null? formals)
			  acc)
			 ((pair? formals)
			  (and (identifier? (car formals))
			       (loop (cdr formals) `(,(car formals) . ,acc))))
			 (else
			  (and (identifier? formals)
			       `(,formals . ,acc)))))))
	    (unless formal-list
	      (error "invalid formal arguments" formals))
	    (let ((new-env (extend-environment formal-list env)))
	      `(lambda ,(let rec ((formals formals))
			  (cond
			   ((null? formals)
			    '())
			   ((pair? formals)
			    `(,(expand (car formals) new-env) . ,(rec (cdr formals))))
			   (else
			    (expand formals new-env))))
		 ,@(let ((body (map (lambda (form) (expand form new-env)) body)))
		     (define (expand-definition form)
		       (cond
			((not (pair? form)))
			((eq? (car form) 'quote))
			((eq? (car form) 'lambda))
			((eq? (car form) 'define)
			 (let ((body (expand (list-ref form 2) new-env)))
			   (list-set! form 2 body)
			   (expand-definition body)))
			(else
			 (for-each expand-definition form))))

		     (for-each expand-definition body)

		     (let ()
		       (define (definition? form)
			 (cond
			  ((not (pair? form)) #f)
			  ((eq? (car form) 'define))
			  ((eq? (car form) 'define-record-type))
			  ((eq? (car form) 'begin) (every definition? (cdr form)))
			  (else #f)))

		       (define (splice-definition definition)
			 (case (car definition)
			   ((define define-record-type) `(,definition))
			   (else (append-map splice-definition (cdr definition)))))

		       (let loop ((rest body) (definitions '()))
			 (cond
			  ((null? rest)
			   (error "expression required" (last body)))
			  ((definition? (car rest))
			   (loop (cdr rest)
				 `(,(splice-definition (car rest)) . ,definitions)))
			  (else
			   (append (apply append (reverse definitions)) rest))))))))))))

    (install-keyword! 'define*
      (lambda (form env)
	(unless (and (= (length form) 3)
		     (identifier? (cadr form)))
	  (error "malformed define" form))
	(let ((formal (cadr form))
	      (expr (caddr form)))
	  (extend-environment! formal env)
	  `(define ,(expand formal env)
	     ,(if (toplevel-environment? env)
		  (expand expr env)
		  expr)))))		; expand later on

    (install-keyword! 'define-record-type
      (lambda (form env)
	(unless (and (>= (length form) 4)
		     (identifier? (list-ref form 1))
		     (list? (list-ref form 2))
		     (every identifier? (list-ref form 2))
		     (identifier? (list-ref form 3))
		     (every (lambda (field-spec)
			      (and (list? field-spec)
				   (every identifier? field-spec)
				   (let ((l (length field-spec)))
				     (or (= l 2) (= l 3)))))
			    (list-tail form 4))
		     (let ((fields (map car (list-tail form 4))))
		       (every (lambda (formal)
				(memq formal fields))
			      (cdr (list-ref form 2)))))
	  (error "malformed define-record-type" form))
	(let ((type (list-ref form 1))
	      (constructor (car (list-ref form 2)))
	      (formals (cdr (list-ref form 2)))
	      (predicate (list-ref form 3))
	      (field-specs (list-tail form 4)))
	  (extend-environment! type env)
	  (extend-environment! constructor env)
	  (extend-environment! predicate env)
	  (for-each
	   (lambda (field-spec)
	     (extend-environment! (list-ref field-spec 1) env)
	     (when (= (length field-spec) 3)
	       (extend-environment! (list-ref field-spec 2) env)))
	   field-specs)
	  (let ((new-env (extend-environment (map car field-specs) env)))
	    `(define-record-type ,(expand type env)
	       (,(expand constructor env) ,@(map (lambda (formal) (expand formal new-env)) formals))
	       ,(expand predicate env)
	       ,@(map
		  (lambda (field-spec)
		    (if (= (length field-spec) 2)
			`(,(expand (car field-spec) new-env)
			  ,(expand (cadr field-spec) env))
			`(,(expand (car field-spec) new-env)
			  ,(expand (cadr field-spec) env)
			  ,(expand (caddr field-spec) env))))
		  field-specs))))))

    (install-keyword! 'quote
      (lambda (form env)
	(unless (= (length form) 2)
	  (error "malformed quote" form))
	(let ((obj (unwrap-syntax (cadr form))))
	  `',obj)))

    (install-keyword! 'if
      (lambda (form env)
	(case (length form)
	  ((3)
	   `(if ,(expand (cadr form) env)
		,(expand (caddr form) env)))
	  ((4)
	   `(if ,(expand (cadr form) env)
		,(expand (caddr form) env)
		,(expand (cadddr form) env)))
	  (else
	   (error "malformed if" form)))))

    (install-keyword! 'set!
      (lambda (form env)
	(unless (and (= (length form) 3)
		     (identifier? (cadr form)))
	  (error "malformed set!" form))
	`(set! ,(expand (cadr form) env)
	   ,(expand (caddr form) env))))

    (install-keyword! 'begin
      (lambda (form env)
	(let ((forms (cdr form)))
	  `(begin ,@(map (lambda (form) (expand form env)) forms)))))

    (install-keyword! 'parameterize
      (lambda (form env)
	(unless (and (>= (length form) 3)
		     (list? (cadr form))
		     (every (lambda (binding)
			      (= (length binding) 2))
			    (cadr form)))
	  (error "malformed parameterize" form))
	`(parameterize ,(map (lambda (binding)
			       (list (expand (car binding) env)
				     (expand (cadr binding) env)))
			     (cadr form))
	   ,(expand `((,(make-identifier 'lambda (current-meta-environment)) ()
		       ,@(cddr form)))
		    env))))

    (let ()
      (define (interpret-transformer-spec spec env)
	(cond ((and (identifier? (car spec))
		    (identifier=? (car spec) env 'syntax-rules (current-meta-environment)))
	       (make-expander (interpret-syntax-rules spec) env))
	      (else
	       (error "unknown transformer spec" spec))))

      (define (interpret-syntax-rules spec)
	(er-macro-transformer
	 (lambda (form rename compare)

	   ;; missing features:
	   ;; - placeholder
	   ;; - vector
	   ;; - more syntax check (e.g. non-linearity of pattern variables)

	   (define-values (ellipsis literals rules)
	     (if (list? (cadr spec))
		 (values (make-identifier '... (current-meta-environment)) (cadr spec) (cddr spec))
		 (values (cadr spec) (caddr spec) (cdddr spec))))

	   ;; p ::= var | constant | (p <ellipsis> . p) | (p . p)

	   (define-syntax case-pattern
	     (syntax-rules (variable-pattern constant-pattern ellipsis-pattern pair-pattern)
	       ((_ pat
		   ((variable-pattern var) . var-body)
		   ((constant-pattern obj) . const-body)
		   ((ellipsis-pattern rep succ) . ellipsis-body)
		   ((pair-pattern head tail) . pair-body))
		(let ((tmp pat))
		  (cond ((identifier? tmp) (let ((var tmp)) . var-body))
			((not (pair? tmp)) (let ((obj tmp)) . const-body))
			((and (pair? (cdr pat))
			      (identifier? (cadr pat))
			      (compare (cadr pat) ellipsis))
			 (let ((rep (car pat)) (succ (cddr pat))) . ellipsis-body))
			(else (let ((head (car tmp)) (tail (cdr tmp))) . pair-body)))))))

	   (define (pattern-variables pat) ; pattern -> ((var . depth))
	     (let go ((pat pat) (depth 0) (acc '()))
	       (case-pattern pat
		 ((variable-pattern var) (alist-cons var depth acc))
		 ((constant-pattern obj) acc)
		 ((ellipsis-pattern rep-pat succ-pat) (go rep-pat (+ depth 1) (go succ-pat depth acc)))
		 ((pair-pattern car-pat cdr-pat) (go car-pat depth (go cdr-pat depth acc))))))

	   (define (syntax-check pattern template) ; pattern * template -> undefined
	     (let ((pattern-variables (pattern-variables pattern))
		   (template-variables (pattern-variables template)))
	       (for-each
		(lambda (var-depth-in-template)
		  (let ((var (car var-depth-in-template)))
		    (let ((var-depth-in-pattern (assq var pattern-variables)))
		      (when var-depth-in-pattern
			(unless (= (cdr var-depth-in-template) (cdr var-depth-in-pattern))
			  (error "syntax-rules: malformed rule"
				 `(,pattern ,template)
				 (unwrap-syntax (car var-depth-in-template))))))))
		template-variables)))

	   (define (pattern-match pat form) ; pattern * obj -> ((var . obj))
	     (call/cc
	      (lambda (return)
		(let match ((pat pat) (form form))
		  (let* ((acc '()) (push! (lambda (x) (set! acc (cons x acc)))))
		    (let walk ((pat pat) (form form))
		      (case-pattern pat
			((variable-pattern var)
			 (if (memq var literals) ; comparing literal identifiers using eq?
			     (unless (and (identifier? form)
					  (compare form (rename var)))
			       (return #f))
			     (push! `(,var . ,form))))
			((constant-pattern obj)
			 (unless (equal? pat form)
			   (return #f)))
			((ellipsis-pattern rep-pat succ-pat)
			 (let ()
			   (define (reverse* x)
			     (let loop ((x x) (acc '()))
			       (if (pair? x)
				   (loop (cdr x) (cons (car x) acc))
				   (values acc x))))
			   (let-values (((rev-pat last-pat) (reverse* succ-pat))
					((rev-form last-form) (reverse* form)))
			     (walk last-pat last-form)
			     (let ((rep-form (let loop ((rev-pat rev-pat) (rev-form rev-form))
					       (cond ((null? rev-pat) (reverse rev-form))
						     ((null? rev-form) (return #f))
						     (else (walk (car rev-pat) (car rev-form))
							   (loop (cdr rev-pat) (cdr rev-form)))))))
			       (if (null? rep-form)
				   (let ((variables (map car (pattern-variables rep-pat))))
				     (for-each
				      (lambda (var)
					(push! `(,var . ())))
				      variables))
				   (let ((substs (map (lambda (obj) (match rep-pat obj)) rep-form)))
				     (let ((variables (map car (car substs))))
				       (for-each
					(lambda (var)
					  (push! `(,var . ,(map (lambda (subst) (cdr (assq var subst))) substs))))
					variables))))))))
			((pair-pattern car-pat cdr-pat)
			 (unless (pair? form)
			   (return #f))
			 (walk car-pat (car form))
			 (walk cdr-pat (cdr form)))))
		    acc)))))

	   (define (rewrite-template template subst) ; template * ((var . obj)) -> obj
	     (let rewrite ((template template))
	       (case-pattern template
		 ((variable-pattern var)
		  (cond
		   ((assq var subst) => cdr)
		   (else (rename var))))
		 ((constant-pattern obj)
		  obj)
		 ((ellipsis-pattern rep-templ succ-templ)
		  (let ((vars-in-templ (map car (pattern-variables rep-templ))))
		    (let ((vars-to-unroll (filter (lambda (var) (assq var subst)) vars-in-templ)))
		      (let ((vals-to-unroll (map (lambda (var) (cdr (assq var subst))) vars-to-unroll)))
			(let ((new-substs (apply map (lambda vals (map cons vars-to-unroll vals)) vals-to-unroll)))
			  (append (map (lambda (subst) (rewrite-template rep-templ subst)) new-substs)
				  (rewrite succ-templ)))))))
		 ((pair-pattern car-templ cdr-templ)
		  (cons (rewrite car-templ)
			(rewrite cdr-templ))))))

	   (let loop ((rules rules))
	     (if (null? rules)
		 (error "no rule matched" form)
		 (let ((rule (car rules)))
		   (let ((pattern (car rule))
			 (template (cadr rule)))
		     (syntax-check pattern template)
		     (let ((subst (pattern-match pattern form)))
		       (if subst
			   (rewrite-template template subst)
			   (loop (cdr rules)))))))))))

      (install-keyword! 'let-syntax
	(lambda (form env)
	  (let ((bindings (cadr form))
		(body (cddr form)))
	    (let ((keywords (map car bindings))
		  (transformer-specs (map cadr bindings)))
	      (let ((expanders (map (lambda (spec) (interpret-transformer-spec spec env)) transformer-specs)))
		(let ((new-env (extend-environment '() env)))
		  (for-each
		   (lambda (keyword expander)
		     (install-expander! keyword expander new-env))
		   keywords expanders)	    
		  (expand `((,(make-identifier 'lambda (current-meta-environment)) () ,@body)) new-env)))))))

      (install-keyword! 'letrec-syntax
	(lambda (form env)
	  (let ((bindings (cadr form))
		(body (cddr form)))
	    (let ((keywords (map car bindings))
		  (transformer-specs (map cadr bindings)))
	      (let ((new-env (extend-environment '() env)))
		(let ((expanders (map (lambda (spec) (interpret-transformer-spec spec new-env)) transformer-specs)))
		  (for-each
		   (lambda (keyword expander)
		     (install-expander! keyword expander new-env))
		   keywords expanders)
		  (expand `((,(make-identifier 'lambda (current-meta-environment)) () ,@body)) new-env)))))))

      (install-keyword! 'define-syntax
	(lambda (form env)
	  (let ((keyword (cadr form))
		(transformer-spec (caddr form)))
	    (let ((expander (interpret-transformer-spec transformer-spec env)))
	      (install-expander! keyword expander env)
	      '(begin)))))

      (install-keyword! 'syntax-error
	(lambda (form _)
	  (unless (and (>= (length form) 2)
		       (string? (cadr form)))
	    (error "malformed syntax-error" form))
	  (apply error (cdr form)))))

    (install-keyword! 'include
      (er-macro-transformer
       (lambda (form rename compare)
	 (unless (every string? (cdr form))
	   (error "malformed include" form))
	 (let ((forms (let loop ((filenames (cdr form)) (acc '()))
			(if (null? filenames)
			    (reverse acc)
			    (loop (cdr filenames)
				  (call-with-input-file (car filenames)
				    (lambda (port)
				      (let loop ((form (read port)) (acc acc))
					(if (eof-object? form)
					    acc
					    (loop (read port) (cons form acc)))))))))))
	   `(,(rename 'begin) ,@forms)))))

    (install-keyword! 'if-expand
      (er-macro-transformer
       (lambda (form rename compare)
	 (unless (= (length form) 4)
	   (error "malformed if-expand" form))
	 (let ((condition (cadr form)))
	   (if (and (pair? condition)
		    (compare (car condition) (rename 'library)))
	       (if (library-exists? (unwrap-syntax (cadr condition)))
		   (list-ref form 2)
		   (list-ref form 3))
	       (if (memq (unwrap-syntax condition) feature-list)
		   (list-ref form 2)
		   (list-ref form 3)))))))

    (expand-toplevel
     '((define-syntax define
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

       (define-syntax cond-expand	; follows R7RS semantics
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

       (define-syntax define-values-aux	; use define-syntax because letrec-syntax creates an environment
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
	    (define-values-aux () (tmp ... . new-tmp) ((x new-tmp) . bindings) init))))))))

(set! feature-list (cons 'r7rs feature-list))

(make-library '(scheme cxr))
(with-library '(scheme cxr)
  (lambda ()
    (for-each library-export
	      '(caaar caadr cadar caddr
		cdaar cdadr cddar cdddr
		caaaar caaadr caadar caaddr
		cadaar cadadr caddar cadddr
		cdaaar cdaadr cdadar cdaddr
		cddaar cddadr cdddar cddddr))
    (for-each import-native
	      '(caaar caadr cadar caddr
		cdaar cdadr cddar cdddr
		caaaar caaadr caadar caaddr
		cadaar cadadr caddar cadddr
		cdaaar cdaadr cdadar cdaddr
		cddaar cddadr cdddar cddddr))))

(make-library '(scheme write))
(with-library '(scheme write)
  (lambda ()
    (for-each library-export '(write write-simple write-shared display))
    (for-each import-native '(write write-simple write-shared display))))

(make-library '(scheme read))
(with-library '(scheme read)
  (lambda ()
    (library-export 'read)
    (import-native 'read)))

(make-library '(scheme file))
(with-library '(scheme file)
  (lambda ()
    (for-each library-export '(call-with-input-file call-with-output-file
				delete-file file-exists?
				open-binary-input-file open-binary-output-file
				open-input-file open-output-file
				with-input-from-file with-output-to-file))
    (for-each import-native '(call-with-input-file call-with-output-file
				delete-file file-exists?
				open-binary-input-file open-binary-output-file
				open-input-file open-output-file
				with-input-from-file with-output-to-file))))

(make-library '(scheme process-context))
(with-library '(scheme process-context)
  (lambda ()
    (for-each library-export '(command-line
			       emergency-exit
			       exit
			       get-environment-variable
			       get-environment-variables))
    (for-each import-native '(command-line
			       emergency-exit
			       exit
			       get-environment-variable
			       get-environment-variables))))

(make-library '(scheme case-lambda))
(with-library '(scheme case-lambda)
  (lambda ()
    (library-export 'case-lambda)
    (library-import '(scheme base))
    (install-keyword! 'case-lambda
      (lambda (form env)
	`(case-lambda
	  ,@(map (lambda (formal-body)
		   (cdr (expand `(,(make-identifier 'lambda (current-meta-environment))
				  ,@formal-body)
				env)))
		 (cdr form)))))))

(make-library '(srfi 1))
(with-library '(srfi 1)
  (lambda ()
    (for-each library-export '(xcons list-tabulate cons* 
			       proper-list? circular-list? dotted-list? not-pair? null-list? list=
			       circular-list length+
			       iota
			       first second third fourth fifth sixth seventh eighth ninth tenth
			       car+cdr
			       take       drop       
			       take-right drop-right 
			       take!      drop-right!
			       split-at   split-at!
			       last last-pair
			       zip unzip1 unzip2 unzip3 unzip4 unzip5
			       count
			       append! append-reverse append-reverse! concatenate concatenate! 
			       unfold       fold       pair-fold       reduce
			       unfold-right fold-right pair-fold-right reduce-right
			       append-map append-map! map! pair-for-each filter-map map-in-order
			       filter  partition  remove
			       filter! partition! remove! 
			       find find-tail any every list-index
			       take-while drop-while take-while!
			       span break span! break!
			       delete delete!
			       alist-cons alist-copy
			       delete-duplicates delete-duplicates!
			       alist-delete alist-delete!
			       reverse! 
			       lset<= lset= lset-adjoin  
			       lset-union  lset-intersection  lset-difference  lset-xor  lset-diff+intersection
			       lset-union! lset-intersection! lset-difference! lset-xor! lset-diff+intersection!))

    (for-each import-native '(xcons list-tabulate cons* 
			      proper-list? circular-list? dotted-list? not-pair? null-list? list=
			      circular-list length+
			      iota
			      first second third fourth fifth sixth seventh eighth ninth tenth
			      car+cdr
			      take       drop       
			      take-right drop-right 
			      take!      drop-right!
			      split-at   split-at!
			      last last-pair
			      zip unzip1 unzip2 unzip3 unzip4 unzip5
			      count
			      append! append-reverse append-reverse! concatenate concatenate! 
			      unfold       fold       pair-fold       reduce
			      unfold-right fold-right pair-fold-right reduce-right
			      append-map append-map! map! pair-for-each filter-map map-in-order
			      filter  partition  remove
			      filter! partition! remove! 
			      find find-tail any every list-index
			      take-while drop-while take-while!
			      span break span! break!
			      delete delete!
			      alist-cons alist-copy
			      delete-duplicates delete-duplicates!
			      alist-delete alist-delete!
			      reverse! 
			      lset<= lset= lset-adjoin  
			      lset-union  lset-intersection  lset-difference  lset-xor  lset-diff+intersection
			      lset-union! lset-intersection! lset-difference! lset-xor! lset-diff+intersection!))))
