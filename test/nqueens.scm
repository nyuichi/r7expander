;;; NQUEENS -- Compute number of solutions to 8-queens problem.

(import (scheme base)
	(scheme write))

(define (nqueens n)

  (define (dec-to n)
    (let loop ((i n) (l '()))
      (if (= i 0) l (loop (- i 1) (cons i l)))))

  (define (try x y z)
    (if (null? x)
	(if (null? y)
            1
            0)
	(+ (if (ok? (car x) 1 z)
               (try (append (cdr x) y) '() (cons (car x) z))
               0)
           (try (cdr x) (cons (car x) y) z))))

  (define (ok? row dist placed)
    (if (null? placed)
	#t
	(and (not (= (car placed) (+ row dist)))
             (not (= (car placed) (- row dist)))
             (ok? row (+ dist 1) (cdr placed)))))

  (try (dec-to n) '() '()))

(display (nqueens 8))
(newline)

;;; This program is taken from https://wiki.call-cc.org/chicken-compilation-process.
