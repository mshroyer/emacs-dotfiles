.PHONY: magit helm with-editor

all: magit helm with-editor

magit: with-editor
	cd magit && $(MAKE) lisp

helm:
	cd helm && $(MAKE)

with-editor:
	cd with-editor && $(MAKE) lisp
