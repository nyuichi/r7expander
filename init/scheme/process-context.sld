(define-library (scheme process-context)
  (export command-line
	  emergency-exit
	  exit
	  get-environment-variable
	  get-environment-variables)
  (import (only (r7expander native)
		command-line
		emergency-exit
		exit
		get-environment-variable
		get-environment-variables)))
