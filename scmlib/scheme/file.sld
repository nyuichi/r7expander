(define-library (scheme file)
  (export call-with-input-file call-with-output-file
	  delete-file file-exists?
	  open-binary-input-file open-binary-output-file
	  open-input-file open-output-file
	  with-input-from-file with-output-to-file)
  (import (only (r7expander native)
		call-with-input-file call-with-output-file
		delete-file file-exists?
		open-binary-input-file open-binary-output-file
		open-input-file open-output-file
		with-input-from-file with-output-to-file)))
