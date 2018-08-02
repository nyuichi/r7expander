(define-library (foo bar)
  (import (scheme base))
  (begin
    (define x 1)
    (let ((y 2))
      (+ x y))))
