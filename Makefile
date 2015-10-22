.PHONY: submodules

all: submodules

submodules:
	cd submodules && $(MAKE)
