.PHONY: magit helm with-editor lua-mode slime

current_dir=$(shell pwd)

all: magit helm lua-mode slime

magit: with-editor
	cd magit && env LOAD_PATH="-L . -L $(current_dir)/dash -L $(current_dir)/with-editor" $(MAKE) lisp

helm:
	cd helm && $(MAKE)

with-editor:
	cd with-editor && $(MAKE) lisp

lua-mode:
	cd lua-mode && $(MAKE)

slime:
	cd slime && $(MAKE)
