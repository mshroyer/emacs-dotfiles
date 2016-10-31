.PHONY: magit helm with-editor lua-mode

all: magit helm lua-mode

magit: with-editor
	cd magit && $(MAKE) lisp

helm:
	cd helm && $(MAKE)

with-editor:
	cd with-editor && $(MAKE) lisp

lua-mode:
	cd lua-mode && $(MAKE)
