;;; Hygienic macro expander

(define-library (r7expander syntactic-closure)
  (export syntactic-closure? make-syntactic-closure
	  make-identifier identifier? identifier=?
	  close-syntax unwrap-syntax
	  assq-environment extend-environment extend-environment!
	  expander? make-expander install-expander!
	  make-toplevel-environment toplevel-environment?
	  current-toplevel-environment with-toplevel-environment
	  install-toplevel-binding!
	  current-meta-environment with-meta-environment
	  expand
	  capture-syntactic-environment sc-macro-transformer rsc-macro-transformer
	  er-macro-transformer)
  (import (scheme base)
	  (scheme case-lambda)
	  (srfi 1))
  (begin
    (define-record-type environment
      (make-environment base frame renamer) ; at toplevel all symbols (but identifiers) are bound
      environment?
      (base enclosing-environment)
      (frame environment-frame set-environment-frame!)
      (renamer environment-renamer))

    (define (make-toplevel-environment renamer)
      (make-environment #f '() renamer))

    (define (toplevel-environment? env)
      (not (enclosing-environment env)))

    (define current-toplevel-environment
      (make-parameter #f))

    (define (with-toplevel-environment top-env thunk)
      (parameterize ((current-toplevel-environment top-env))
	(thunk)))

    (define (assq-environment id env)
      (let ((frame (environment-frame env)))
	(or (assq id frame)
	    (if (toplevel-environment? env)
		(and (symbol? id)
		     (let ((new-name ((environment-renamer env) id)))
		       (set-environment-frame! env (alist-cons id new-name frame))
		       (assq-environment id env)))
		(assq-environment id (enclosing-environment env))))))

    (define (install-toplevel-binding! id name top-env)
      (let ((frame (environment-frame top-env)))
	(set-environment-frame! top-env (alist-cons id name frame))))

    (define-record-type syntactic-closure-type
      (make-syntactic-closure env free form)
      syntactic-closure?
      (env syntactic-closure-environment)
      (free syntactic-closure-free-names)
      (form syntactic-closure-form))

    (define (close-syntax form env)
      (make-syntactic-closure env '() form))

    (define (unwrap-syntax obj)
      (cond
       ((syntactic-closure? obj)
	(unwrap-syntax (syntactic-closure-form obj)))
       ((pair? obj)
	(cons (unwrap-syntax (car obj)) (unwrap-syntax (cdr obj))))
       ((vector? obj)
	(vector-map unwrap-syntax obj))
       (else
	obj)))

    (define (make-identifier id env)
      (close-syntax id env))

    (define (identifier? obj)
      (or (symbol? obj)
	  (and (syntactic-closure? obj)
	       (identifier? (syntactic-closure-form obj)))))

    (define (identifier=? id1 env1 id2 env2)
      (eq? (expand id1 env1) (expand id2 env2)))

    (define generate-name
      (let ((n 0))
	(lambda (id)
	  (let ((m n))
	    (set! n (+ n 1))
	    (string->symbol
	     (string-append
	      "%"
	      (symbol->string (unwrap-syntax id))
	      "."
	      (number->string m)))))))

    (define (extend-environment! id env)
      (unless (and (toplevel-environment? env) (symbol? id))
	(let ((frame (environment-frame env)))
	  (cond
	   ((assq id frame)
	    (error "duplicate binding" id))
	   (else
	    (let ((name (generate-name id)))
	      (set-environment-frame! env (alist-cons id name frame))))))))

    (define (extend-environment ids env)
      (let ((new-env (make-environment env '() #f)))
	(for-each
	 (lambda (id)
	   (extend-environment! id new-env))
	 ids)
	new-env))

    (define-record-type expander
      (make-expander transformer environment) expander?
      (transformer expander-transformer)
      (environment expander-environment))

    (define (install-expander! keyword expander env)
      (extend-environment! keyword env)
      (let ((cell (assq-environment keyword env)))
	(set-cdr! cell expander)))

    (define current-meta-environment
      (make-parameter #f))

    (define (with-meta-environment meta-env thunk)
      (parameterize ((current-meta-environment meta-env))
	(thunk)))

    (define expand
      (let ()
	(define (expand form env)
	  (let expand ((form form))
	    (cond
	     ((identifier? form) (expand-identifier form env))
	     ((syntactic-closure? form) (expand-syntactic-closure form env))
	     ((and (pair? form) (list? form))
	      (if (identifier? (car form))
		  (let ((e (expand (car form))))
		    (if (expander? e)
			(expand-macro e form env)
			`(,e ,@(map expand (cdr form)))))
		  (map expand form)))
	     ((not (pair? form)) form)
	     (else
	      (error "invalid expression" form)))))

	(define (expand-macro expander form env)
	  (let ((transformer (expander-transformer expander))
		(meta-env (expander-environment expander)))
	    (with-meta-environment meta-env
	      (lambda ()
		(transformer form env)))))

	(define (expand-syntactic-closure sc env)
	  (expand (syntactic-closure-form sc)
		  (make-environment
		    (syntactic-closure-environment sc)
		    (map (lambda (id) `(,id . ,(expand id env)))
			 (syntactic-closure-free-names sc))
		    #f)))

	(define (expand-identifier id env)
	  (cond ((assq-environment id env) => cdr)
		(else (expand-identifier
		       (syntactic-closure-form id)
		       (syntactic-closure-environment id)))))

	(case-lambda
	 ((form)
	  (expand form (current-toplevel-environment)))
	 ((form env)
	  (expand form env)))))

    (define (make-singular-form proc)
      (let ((expander (make-expander (lambda (form env) (proc env)) (current-meta-environment))))
	(let ((env (make-toplevel-environment (lambda (id) (error "logic flaw")))))
	  (extend-environment! 'foo env)
	  (install-expander! 'foo expander env)
	  `(,(make-identifier 'foo env)))))

    (define (capture-syntactic-environment proc)
      (make-singular-form
       (lambda (env)
	 (expand (proc env) env))))

    (define (sc-macro-transformer proc)
      (lambda (form env)
	(expand (proc form env) (current-meta-environment))))

    (define (rsc-macro-transformer proc)
      (lambda (form env)
	(expand (proc form (current-meta-environment)) env)))

    (define (er-macro-transformer proc)
      (lambda (form env)
	(let ((table '()))
	  (let ((rename (lambda (x)
			  (cond
			   ((assq x table) => cdr)
			   (else (let ((id (make-identifier x (current-meta-environment))))
				   (set! table (alist-cons x id table))
				   id)))))
		(compare (lambda (x y)
			   (identifier=? x env y env))))
	    (expand (proc form rename compare) env)))))))

;;; Local Variables:
;;; eval: (put 'with-toplevel-environment 'scheme-indent-function 1)
;;; eval: (put 'with-meta-environment 'scheme-indent-function 1)
;;; End:
