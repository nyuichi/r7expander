(make-library '(r7expander builtin))
(with-library '(r7expander builtin)
  (lambda ()
    (define (install-builtin! keyword transformer)
      (let ((env (current-toplevel-environment)))
	(let ((expander (make-expander transformer env)))
	  (install-expander! keyword expander env)))
      (library-export keyword))

    (for-each library-export '(syntax-rules _ ...))

    (install-builtin! 'lambda
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

    (install-builtin! 'define
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

    (install-builtin! 'define-record-type
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

    (install-builtin! 'quote
      (lambda (form env)
	(unless (= (length form) 2)
	  (error "malformed quote" form))
	(let ((obj (unwrap-syntax (cadr form))))
	  `',obj)))

    (install-builtin! 'if
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

    (install-builtin! 'set!
      (lambda (form env)
	(unless (and (= (length form) 3)
		     (identifier? (cadr form)))
	  (error "malformed set!" form))
	`(set! ,(expand (cadr form) env)
	       ,(expand (caddr form) env))))

    (install-builtin! 'begin
      (lambda (form env)
	(let ((forms (cdr form)))
	  `(begin ,@(map (lambda (form) (expand form env)) forms)))))

    (install-builtin! 'parameterize
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

      (install-builtin! 'let-syntax
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

      (install-builtin! 'letrec-syntax
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

      (install-builtin! 'define-syntax
	(lambda (form env)
	  (let ((keyword (cadr form))
		(transformer-spec (caddr form)))
	    (let ((expander (interpret-transformer-spec transformer-spec env)))
	      (install-expander! keyword expander env)
	      '(begin)))))

      (install-builtin! 'syntax-error
	(lambda (form _)
	  (unless (and (>= (length form) 2)
		       (string? (cadr form)))
	    (error "malformed syntax-error" form))
	  (apply error (cdr form)))))

    (install-builtin! 'include
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

    (install-builtin! 'if-expand
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

    (install-builtin! 'case-lambda
      (lambda (form env)
	`(case-lambda
	  ,@(map (lambda (formal-body)
		   (cdr (expand `(,(make-identifier 'lambda (current-meta-environment))
				  ,@formal-body)
				env)))
		 (cdr form)))))))

(make-library '(r7expander native))
(with-library '(r7expander native)
  (lambda ()
    (define (install-native! keyword)
      (let ((env (current-toplevel-environment)))
	(install-toplevel-binding! keyword keyword env))
      (library-export keyword))

    (for-each install-native!
	      '(;; (scheme base)
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
		flush-output-port
		;; (scheme cxr)
		caaar caadr cadar caddr
		cdaar cdadr cddar cdddr
		caaaar caaadr caadar caaddr
		cadaar cadadr caddar cadddr
		cdaaar cdaadr cdadar cdaddr
		cddaar cddadr cdddar cddddr
		;; (scheme file)
		call-with-input-file call-with-output-file
		delete-file file-exists?
		open-binary-input-file open-binary-output-file
		open-input-file open-output-file
		with-input-from-file with-output-to-file
		;; (scheme process-context)
		command-line
		emergency-exit
		exit
		get-environment-variable
		get-environment-variables
		;; (scheme read)
		read
		;; (scheme write)
		write write-simple write-shared display))))

(load-library-from-file "init/scheme/base.sld")
(set! feature-list (cons 'r7rs feature-list))
(load-library-from-file "init/scheme/case-lambda.sld")
(load-library-from-file "init/scheme/cxr.sld")
(load-library-from-file "init/scheme/file.sld")
(load-library-from-file "init/scheme/process-context.sld")
(load-library-from-file "init/scheme/read.sld")
(load-library-from-file "init/scheme/write.sld")
