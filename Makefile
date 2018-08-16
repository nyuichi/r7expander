GOSH = env GAUCHE_KEYWORD_IS_SYMBOL=1 gosh
RUN_SCRIPT = $(GOSH) -l ./r7expander/syntactic-closure.sld -l ./r7expander/library.sld --
RUN_COMPILED = $(GOSH) -u scheme.base --
OPTS = -l ./extlib/srfi/1.sld -l ./r7expander/syntactic-closure.sld -l ./r7expander/library.sld main.scm

r7expander.scm: r7expander/syntactic-closure.sld r7expander/library.sld extlib/srfi/1.sld extlib/pp.scm init/*/*.sld init.scm main.scm
	$(RUN_SCRIPT) ./main.scm $(OPTS) > expander1.scm
	$(RUN_COMPILED) ./expander1.scm $(OPTS) > expander2.scm
	diff expander1.scm expander2.scm || { echo "compilation results unmatched"; exit 1; }
	echo "#!/usr/bin/env GAUCHE_KEYWORD_IS_SYMBOL=1 gosh -u scheme.base -u srfi.1 --" > r7expander.scm
	cat expander1.scm >> r7expander.scm
	chmod +x r7expander.scm
	$(RM) expander1.scm expander2.scm

clean:
	$(RM) ./r7expander.scm

.PHONY: clean

