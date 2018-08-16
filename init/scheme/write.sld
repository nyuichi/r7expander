(define-library (scheme write)
  (export write
	  write-simple
	  write-shared
	  display)
  (import (only (r7expander native)
		write
		write-simple
		write-shared
		display)))
