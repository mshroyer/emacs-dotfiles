Mark Shroyer's Emacs Dotfiles
=============================

This is my global Emacs configuration directory, `~/.emacs.d/`.  It
contains my Emacs settings and abbreviation files, as well as numerous
extensions for the editor which I have either copied from elsewhere or
written myself.


Installation
------------

Aside from checking out this repository to `~/.emacs.d/`, no special
installation actions are required on Unix-like systems.  However, make sure
you don't have an old `~/.emacs` or `~/.emacs.el` file sitting around on
your system, as this will supersede `~/.emacs.d/init.el` in GNU Emacs's
search path.

Also, if you're installing on Windows, set the environment variable HOME to
reference your home directory.  If this variable is not set my Emacs
startup script will attempt to infer it from the Windows HOMEDRIVE and
HOMEPATH variables, but I don't know how reliable this method is.


External Dependencies
---------------------

There are several optional, third-party Emacs extensions that I use on my
main computer, but which are not included directly in this repository
because I want to keep down the repository's size for computers where these
extensions are not necessary, and also so that I can keep these externals
up to date with their latest upstream versions.

The Perl 5 script `externals.pl` in the `bin/` subdirectory can be used to
selectively install these optional externals, as well as to update all
presently installed such externals.  (Unfortunately I couldn't just use git
submodules here because many of these external features are stored in other
version control systems.)  To install externals, change to your newly
checked-out `~/.emacs.d` directory and run the script with the `checkout`
command:

    $ cd ~/.emacs.d
    $ bin/externals.pl checkout

And then follow the directions on the screen.  Update installed externals
as follows:

    $ cd ~/.emacs.d
    $ bin/externals.pl update

**Note:** I've found that CVSNT badly mangles the line endings of checkouts
from the SLIME CVS repository, resulting in the checked out Emacs Lisp
sources being saved in a line ending mode conflicting with the file mode
specified in `slime.el`'s file variables and thereby causing errors in the
Emacs startup script.  For this reason, I recommend running this script
from within Cygwin if you're running Windows, at least as far as CVS is
concerned.


Local Customizations
--------------------

My startup script will look for a file named `~/.emacs.local.el`; if it is
found, this file will be loaded during Emacs startup.  This provides a
method for making machine-local customizations to the Emacs startup.

Provided below is a collection of some local customization recipes that may
prove useful:

### Default coding system for new files ###

    (setf (default-value 'buffer-file-coding-system) 'utf-8-unix)

### Common Lisp implementation for SLIME ###

    (when (featurep 'slime)
      (setq inferior-lisp-program "clisp")
      (add-to-list 'slime-lisp-implementations '(clisp ("clisp"))))

### Tramp settings compatible with both Cygwin and NT Emacs ###

    (if (eql system-type 'windows-nt)
	(setq tramp-default-method "plink")
      (setq tramp-default-method "ssh"))

### Specify edit servers to run ###

    (setq local-server-selection '(:emacs :chrome-edit))

### Spell checker ###

    (setq ispell-program-name "C:/cygwin/bin/aspell.exe"
	  ispell-list-command "list"
	  ispell-extra-args '("--sug-mode=ultra"))

### PostScript printing on Windows ###

Adjust `ps-lpr-command` for your machine's `gswin32c.exe` executable path.

    (setq ps-lpr-command "C:/Program Files/gs/gs8.64/bin/gswin32c.exe"
	  ps-lpr-switches '("-q" "-dNOPAUSE" "-dBATCH" "-sDEVICE=mswinpr2")
	  ps-printer-name t)

### Geographic details ###

    (setq calendar-longitude -80.39
	  calendar-latitude 27.64
	  calendar-location-name "Vero Beach, FL")

    (setq calendar-time-zone -300
	  calendar-standard-time-zone-name "EST"
	  calendar-daylight-time-zone-name "EDT")

### Emacs frame styling ###

    (setq default-frame-alist '((width . 90)
				(height . 46)
				(font . "Consolas-10.5")))

### Enable a color theme ###

    (setq color-theme-local 'color-theme-classic)

### M-x sudoku skill level ###

    (setq sudoku-level "medium")
