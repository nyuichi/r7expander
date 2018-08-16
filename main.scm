;;; Run the script with either of the followings:
;;; 1. $ env GAUCHE_KEYWORD_IS_SYMBOL=1 gosh -l ./r7expander/syntactic-closure.sld -l ./r7expander/library.sld -- ./main.scm
;;; 2. $ csc -R r7rs -prologue r7expander/syntactic-closure.sld -prologue r7expander/library.sld main.scm && ./main

(import (scheme base)
	(scheme file)
	(scheme read)
	(scheme write)
	(scheme process-context)
	(r7expander library))

;;; for init.scm
(import (r7expander syntactic-closure)
	(scheme cxr)
	(srfi 1))

(include "extlib/pp.scm")

(define (file->list filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((form (read port)) (acc '()))
	(if (eof-object? form)
	    (reverse acc)
	    (loop (read port) (cons form acc)))))))

(define (load-library-from-file filename)
  (let ((forms (file->list filename)))
    (unless (and (= (length forms) 1)
		 (list? (car forms))
		 (>= (length (car forms)) 2)
		 (eq? (caar forms) 'define-library))
      (error "malformed library file"))
    (expand-library (car forms))))

(define (load-program-from-file filename)
  (let ((forms (file->list filename)))
    (expand-program forms)))

(include "init.scm")

(define repl-environment
  (make-toplevel-environment
   (lambda (id)
     (string->symbol
      (string-append
       "r7rs.repl:"
       (symbol->string id))))))

(define (start-repl)
  (expand-repl '(import (scheme base)) repl-environment)
  (let loop ()
    (display "> ")
    (flush-output-port)
    (let ((form (read)))
      (unless (eof-object? form)
	(pretty-print (expand-repl form repl-environment))
	(loop)))))

(let loop ((opts (cdr (command-line))))
  (cond
   ((null? opts)
    (start-repl))
   ((equal? (car opts) "-l")
    (pretty-print (load-library-from-file (cadr opts)))
    (loop (cddr opts)))
   ((and (null? (cdr opts)))
    (pretty-print (load-program-from-file (car opts))))
   (else
    (error "invalid command line arguments" opts))))
