;;; R7RS expander

;;; toplevel mutable states:
;;; - library-table
;;; - feature-list

;;; TODO
;;; - check import collision
;;; - check if exported symbols are defined

(define-library (r7expander library)
  (export expand-library expand-program expand-repl expand-toplevel
	  make-library with-library current-library library-exists?
	  library-import library-export
	  feature-list)
  (import (scheme base)
	  (scheme cxr)
	  (scheme read)
	  (scheme file)
	  (srfi 1)
	  (r7expander syntactic-closure))
  (begin
    (define (make-r7rs-toplevel-environment prefix)
      (make-toplevel-environment
       (lambda (id)
	 (string->symbol
	  (string-append
	   prefix
	   (symbol->string id))))))

    (define-record-type library-object
      (make-library-object environment exports) library-object?
      (environment library-object-environment)
      (exports library-object-exports set-library-object-exports!))

    (define library-table
      '())

    (define (assoc-library spec)
      (assoc spec library-table))

    (define current-library
      (make-parameter #f))

    (define (with-library spec thunk)
      (parameterize ((current-library spec))
	(let ((env (library-environment (current-library))))
	  (with-toplevel-environment env thunk))))

    (define make-library
      (let ()
	(define (make-library spec)
	  (let ((env (make-r7rs-toplevel-environment (mangle-library-spec spec))))
	    (let ((obj (make-library-object env '())))
	      (set! library-table (alist-cons spec obj library-table)))))

	(define (mangle-library-spec spec)
	  (let rec ((spec spec))
	    (if (null? spec)
		""
		(string-append
		 (cond
		  ((symbol? (car spec))
		   (symbol->string (car spec)))
		  ((number? (car spec))
		   (number->string (car spec))))
		 (if (null? (cdr spec))
		     ":"
		     (string-append
		      "."
		      (rec (cdr spec))))))))

	make-library))

    (define (library-environment spec)
      (cond ((assoc-library spec) =>
	     (lambda (cell)
	       (library-object-environment (cdr cell))))
	    (else
	     (error "library not found" spec))))

    (define (library-exports spec)
      (cond ((assoc-library spec) =>
	     (lambda (cell)
	       (library-object-exports (cdr cell))))
	    (else
	     (error "library not found" spec))))

    (define (library-exists? spec)
      (and (assoc-library spec) #t))

    (define (expand-library form)
      (let ((spec (cadr form)))
	(make-library spec)
	(with-library spec
	  (lambda ()
	    (let ((decls (cddr form)))
	      (let ((forms (append-map interpret-library-declaration decls)))
		(expand-toplevel forms)))))))

    (define (interpret-library-declaration decl)
      (case (car decl)
	((begin)
	 (cdr decl))
	((import)
	 (for-each library-import (cdr decl))
	 '())
	((export)
	 (for-each library-export (cdr decl))
	 '())
	((cond-expand)
	 (interpret-cond-expand (cdr decl)))
	((include)
	 (files->list (cdr decl)))
	((include-library-declarations)
	 (append-map interpret-library-declaration (files->list (cdr decl))))))

    (define library-import
      (let ()
	(define (library-import spec)
	  (let ((name-map (make-name-map spec)))
	    (let ((env (current-toplevel-environment)))
	      (for-each
	       (lambda (c)
		 (install-toplevel-binding! (car c) (cdr c) env)) ; TODO redefinition of macros
	       name-map))))

	(define (make-name-map spec)
	  (case (car spec)
	    ((prefix)
	     (let ((name-map (make-name-map (cadr spec))))
	       (map (lambda (c)
		      (let ((nickname
			     (string->symbol
			      (string-append
			       (symbol->string (caddr spec))
			       (symbol->string (car c))))))
			(cons nickname (cdr c))))
		    name-map)))
	    ((only)
	     (let ((name-map (make-name-map (cadr spec))))
	       (let ((args (cddr spec)))
		 (map (lambda (v) (assq v name-map)) args))))
	    ((except)
	     (let loop ((name-map (make-name-map (cadr spec))) (args (cddr spec)))
	       (if (null? args)
		   name-map
		   (loop (alist-delete (car args) name-map) (cdr args)))))
	    ((rename)
	     (let loop ((name-map (make-name-map (cadr spec))) (args (cddr spec)))
	       (map (lambda (c)
		      (let loop ((args (cddr spec)))
			(cond
			 ((null? args) c)
			 ((eq? (car c) (caar args)) (cons (cadar args) (cdr c)))
			 (else (loop (cdr args))))))
		    name-map)))
	    (else
	     (let ((exports (library-exports spec))
		   (env (library-environment spec)))
	       (map (lambda (cell)
		      (let ((nickname (car cell)) (id (cdr cell)))
			(let ((name (cdr (assq-environment id env))))
			  `(,nickname . ,name))))
		    exports)))))

	library-import))

    (define (library-export spec)
      (let-values
	  (((id nickname)
	    (if (symbol? spec)
		(values spec spec)
		(values (cadr spec) (caddr spec)))))
	(let ((obj (cdr (assoc-library (current-library)))))
	  (let ((exports (library-object-exports obj)))
	    (set-library-object-exports! obj (alist-cons nickname id exports))))))

    (define feature-list
      '())

    (define (interpret-cond-expand clauses) ; follows srfi-0 semantics
      (let loop ((clauses clauses))
	(if (null? clauses)
	    (error "unfulfilled cond-expand")
	    (let ((c (caar clauses)))
	      (if (or (eq? c 'else)
		      (let test ((c c))
			(if (symbol? c)
			    (memq c feature-list)
			    (case (car c)
			      ((library)
			       (library-exists? (cadr c)))
			      ((not)
			       (not (test (cadr c))))
			      ((and)
			       (let loop ((cs (cdr c)))
				 (or (null? cs)
				     (and (test (car cs))
					  (loop (cdr cs))))))
			      ((or)
			       (let loop ((cs (cdr c)))
				 (and (pair? cs)
				      (or (test (car cs))
					  (loop (cdr cs))))))))))
		  (append-map interpret-library-declaration (cdar clauses))
		  (loop (cdr clauses)))))))

    (define (files->list filenames)
      (let loop ((filenames filenames) (acc '()))
	(if (null? filenames)
	    (reverse acc)
	    (loop (cdr filenames)
		  (call-with-input-file (car filenames)
		    (lambda (port)
		      (let loop ((form (read port)) (acc acc))
			(if (eof-object? form)
			    acc
			    (loop (read port) (cons form acc))))))))))

    (define (expand-program forms)
      (let ((env (make-r7rs-toplevel-environment "r7expander.program:")))
	(with-toplevel-environment env
	  (lambda ()
	    (let loop ((forms forms))
	      (cond
	       ((and (pair? (car forms))
		     (eq? (caar forms) 'import)
		     (eq? (cdr (assq-environment 'import env)) 'r7expander.program:import)) ; FIXME
		(for-each library-import (cdar forms))
		(loop (cdr forms)))
	       (else
		(expand-toplevel forms))))))))

    (define (expand-repl form env)
      (with-toplevel-environment env
	(lambda ()
	  (cond
	   ((and (list? form) (eq? (car form) 'import)) ; FIXME
	    (for-each library-import (cdr form))
	    '(begin))
	   (else
	    (expand-toplevel (list form)))))))

    (define expand-toplevel
      (let ()
	(define (expand-toplevel forms)
	  (let ((forms (map expand forms)))
	    (let ((forms (let flatten ((form `(begin ,@forms)))
			   (if (and (pair? form) (eq? (car form) 'begin))
			       (append-map flatten (cdr form))
			       (list form)))))
	      (let ((forms (map (lambda (form) (post-expand form #t)) forms)))
		(if (= (length forms) 1)
		    (car forms)
		    `(begin . ,forms))))))

	(define (post-expand form allow-definition?)
	  (cond
	   ((symbol? form)
	    form)
	   ((vector? form)
	    `',form)
	   ((expander? form)
	    (error "invalid use of keyword" form))
	   ((not (list? form))
	    form)
	   (else
	    (case (car form)
	      ((quote)
	       form)
	      ((begin)
	       (when (null? (cdr form))
		 (error "malformed begin" form))
	       `(begin ,@(map (lambda (form) (post-expand form #f)) (cdr form))))
	      ((define)
	       (unless allow-definition?
		 (error "invalid definition" form))
	       `(define ,(cadr form) ,(post-expand (caddr form) #f)))
	      ((define-record-type)
	       (unless allow-definition?
		 (error "invalid record type definition" form))
	       form)
	      ((lambda)
	       `(lambda ,(cadr form)
		  ,@(map (lambda (form) (post-expand form #t)) (cddr form))))
	      (else
	       (map (lambda (form) (post-expand form #f)) form))))))

	expand-toplevel))))

;;; Local Variables:
;;; eval: (put 'with-library 'scheme-indent-function 1)
;;; eval: (put 'with-toplevel-environment 'scheme-indent-function 1)
;;; eval: (put 'install-keyword! 'scheme-indent-function 1)
;;; End:
