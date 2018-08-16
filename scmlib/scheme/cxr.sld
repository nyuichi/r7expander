(define-library (scheme cxr)
  (export caaar caadr cadar caddr
	  cdaar cdadr cddar cdddr
	  caaaar caaadr caadar caaddr
	  cadaar cadadr caddar cadddr
	  cdaaar cdaadr cdadar cdaddr
	  cddaar cddadr cdddar cddddr)
  (import (only (r7expander native)
		caaar caadr cadar caddr
		cdaar cdadr cddar cdddr
		caaaar caaadr caadar caaddr
		cadaar cadadr caddar cadddr
		cdaaar cdaadr cdadar cdaddr
		cddaar cddadr cdddar cddddr)))
