.PHONY: magit helm

all: magit helm

magit: with-editor
	cd magit && $(MAKE) lisp

helm:
	cd helm && $(MAKE)

with-editor:
	cd with-editor && $(MAKE) lisp
