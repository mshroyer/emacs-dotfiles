.PHONY: magit helm with-editor lua-mode slime

all: magit helm lua-mode slime

magit: with-editor
	cd magit && $(MAKE) lisp

helm:
	cd helm && $(MAKE)

with-editor:
	cd with-editor && $(MAKE) lisp

lua-mode:
	cd lua-mode && $(MAKE)

slime:
	cd slime && $(MAKE)
