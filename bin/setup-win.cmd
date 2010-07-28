@echo off
setlocal enabledelayedexpansion

REM Setup Windows (Vista or higher) symbolic links for the user's .emacs.d
REM directory.

if "%HOME%" neq "" goto setup
set HOME="%HOMEDRIVE%%HOMEPATH%"

:setup
cd "%HOME%"
mklink .emacs       .emacs.d\emacs
mklink .abbrev_defs .emacs.d\abbrev_defs
