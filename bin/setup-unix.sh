#!/bin/sh

# Setup Unix symbolic links for the ~/.emacs.d directory.

cd ~
ln -s .emacs.d/emacs       .emacs
ln -s .emacs.d/abbrev_defs .abbrev_defs
