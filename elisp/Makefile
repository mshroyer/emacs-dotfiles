# Makefile for my elisp directory.

EMACS = emacs

compile: mshroyer-lib.elc

%.elc: %.el
	@$(EMACS) -batch -q -f batch-byte-compile $<

clean:
	rm -f *.elc

.PHONY: clean
