;;; -*- Mode: Emacs-Lisp; -*-
;;;
;;; ~/.emacs
;;;
;;; My global Emacs configuration file.
;;;
;;; Mark Shroyer
;;; http://markshroyer.com/

(require 'cl)

;;; UTILITY

(defun char (str i)
  "Return character at position i in str"
  (string-to-char (substring str i)))

(defun last-char (str)
  "Return last character in str"
  (char str (- (length str) 1)))

;; Generate a simple list of tab stops, 'stop-width characters in between
;; and up to 'text-width characters long in total.
(defun simple-tab-stop-list (stop-width text-width)
  "Generate a simple list of tab stops"

  (do ((stop stop-width (+ stop stop-width))
        (stop-list nil))
      ((> stop text-width) stop-list)
    (setq stop-list (append stop-list (list stop)))))

;; Install the paredit minor mode as a hook for the given mode name, but
;; only if paredit is available.
(defmacro add-paredit-hook (mode-name)
  (let* ((hook-name (concat (symbol-name mode-name)
                            "-hook"))
         (hook (intern hook-name)))
    `(if (or (featurep 'paredit) (fboundp 'paredit-mode))
         (add-hook (quote ,hook)
                   (lambda ()
                     (paredit-mode t))))))

;; Flattens an assoc-list tree of paths (such as the user-elisp paths)
;; depth-first into a plain list of paths
(defun flatten-path-tree (path-tree)
  (if (null path-tree)
      nil
    (let* ((sub-tree  (car path-tree))
           (this-dir  (if (eql (last-char (car sub-tree))
                               (string-to-char "/"))
                          (car sub-tree)
                        (concat (car sub-tree) "/"))))
      (cons this-dir
            (append (if (not (null (cdr sub-tree)))
                        (mapcar (lambda (sub-path)
                                  (concat this-dir sub-path))
                                (flatten-path-tree (cdr sub-tree))))
                    (flatten-path-tree (cdr path-tree)))))))


;;; SYSTEM

;; The HOME environment variable may not necessarily be set on Windows
;; systems.  If it isn't already set, try to synthesize it from other
;; standard Windows environment variables.
(when (and (or (eql system-type 'windows-nt)
               (eql system-type 'msdos))
           (not (getenv "HOME")))
  (setenv "HOME" "$HOMEDRIVE$HOMEPATH" t))

;; User Emacs directories
(setq user-emacs-directory "~/.emacs.d/"
      generated-autoload-file (concat user-emacs-directory
				      "loaddefs.el"))

;; Tree(s) of paths containing user Emacs Lisp files.  These will be added
;; to the load path, but will not be scanned recursively.
(let ((el-d (concat user-emacs-directory "elisp")))
  (setq user-elisp
        `((,el-d ("slime")
                 ("clojure-mode")
                 ("swank-clojure")
                 ("org-mode/lisp")
                 ("haskellmode-emacs")
                 ("cperl-mode")))))

;; Start server mode if we're running in a windowing environment
(if window-system
    (progn
      (server-start)

      ;; Open files for "emacsclient" in a new frame...
      (add-hook 'server-switch-hook
                (lambda ()
                  (let ((server-buf (current-buffer)))
                    (bury-buffer)
                    (switch-to-buffer-other-frame server-buf))))

      ;; ...and delete that frame once we're done with the file.
      (add-hook 'server-done-hook
                (lambda ()
                  (kill-buffer nil)     ; Close the buffer, too
                  (delete-frame)
                  (redraw-display)      ; Redraw the display to clear
                                        ; messages from main frame minibuf
                  ))))

;; Prepend user elisp directories to the elisp load path.  Then, prepare
;; any autoloads contained in our user load paths.
(let ((my-load-path (remove-if-not #'file-exists-p
                                   (flatten-path-tree user-elisp))))
  (setq load-path (append my-load-path load-path))
  (apply #'update-directory-autoloads my-load-path))


;;; EXTENSIONS

;; Contains autoloads processed from the user-elisp tree.
(load generated-autoload-file)

;; Required features
(require 'paredit)
(require 'git)
(require 'git-blame)

;; Optional features
(require 'slime nil t)
(require 'eperiodic nil t)
(require 'sudoku nil t)
(require 'epa-file nil t)

;; Initialization
(if (featurep 'slime)
    (slime-setup '(inferior-slime)))


;;; LOCAL SETTINGS

;; Retrieve any local configurations from ~/.emacs.local, if the file
;; exists on this system
(let ((local-settings "~/.emacs.local"))
  (if (file-exists-p local-settings)
      (load-file local-settings)))


;;; MAC OS X-SPECIFIC CONFIGURATIONS

;; Use the Option/Alt key for Meta in Emacs.app
(setq mac-command-modifier 'option)
(setq mac-option-modifier 'meta)


;;; FILE HANDLING

;; Delete unnecessary auto-save files
(setq delete-auto-save-files t)

;; Like Vim's ":set backupcopy=yes"; save files in-place so things like
;; Mutt or Cygwin's permissions don't get screwed up.
(setq make-backup-files t
      backup-by-copying t)

;; If we have a ~/.saves directory, keep backup files there and enable
;; backup versioning.
(when (file-exists-p "~/.saves")
  (setq backup-directory-alist '(("." . "~/.saves"))
        version-control t
        kept-new-versions 6
        kept-old-versions 2
        delete-old-versions t))

;; Make sure the last line of a file ends in a carriage return
(setq require-final-newline t)

;; ido mode for switching buffers and finding files
(ido-mode 1)
(setq ido-enable-flex-matching t
      ido-max-directory-size 1000000
      ido-auto-merge-work-directories-length -1)


;;; GENERAL INTERFACE SETTINGS

;; Don't display startup message
(setq inhibit-startup-message t)

;; Get rid of the toolbar, if applicable
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))

;; Don't show the menu bar unless we're running in a window system
(if (not window-system)
    (menu-bar-mode 0))

;; Display current time and load average on mode line
(display-time)

;; Show column number on the mode line
(column-number-mode t)

;; Use visual bell
(setq visible-bell t)

;; Show continuation lines
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)

;; Don't make me type out long answers...
(fset 'yes-or-no-p 'y-or-n-p)

;; Scroll one line at a time, like Vim
(setq scroll-conservatively 2)

;; Show at least three lines of context around the cursor while scrolling
;; (like :set scrolloff=3 in Vim)
(setq scroll-margin 3)

;; Use smooth-scrolling.el for less annoying behavior...
;(require 'smooth-scrolling)
;(setq smooth-scroll-margin 3)

;; Don't use transient mark mode / Zmacs mode (note that this disables
;; region highlighting)
(transient-mark-mode 0)

;; Color themes!  (But only when running in a GUI, of course...)
(if (and window-system
         (boundp 'color-theme-local)
         (not (null color-theme-local)))
    (progn
      (require 'color-theme)            ; Only load themes if one was
      (color-theme-initialize)          ; defined in ~/.emacs.local
      (funcall color-theme-local)))

;; Don't mess around with this disabled commands nonsense
(setq disabled-command-hook nil)

;; Confirm that we want to quit Emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Add a keystroke for renaming a buffer
(global-set-key "\C-cr" 'rename-buffer)


;;; TRAMP

(require 'tramp)
(setq tramp-default-method "scp")


;;; EDITING OPTIONS

;; Viper mode!
;(setq viper-mode t)
;(require 'viper)

;; Text mode abbreviations
(setq-default abbrev-mode t)
(read-abbrev-file "~/.abbrev_defs")
(setq save-abbrevs t)

;; Set up syntax coloring
(global-font-lock-mode 1)

;; Turn on paren matching (this is a Lisp editor, is it not?)
(show-paren-mode 1)
(setq show-paren-style 'mixed)

;; ;; Use spaces for indentation, not tab chracters
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(setq standard-indent 4)
(setq c-indent-level 4)

;; Always use auto-fill in text mode; wrap to 75 characters by default
(setq-default fill-column 75)

;; Show trailing whitespace
(if (>= emacs-major-version 21)
    (setq show-trailing-whitespace t))

;; Swap to C-j for raw newline, C-m for newline-and-indent
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-j" 'newline)
(global-set-key (kbd "<C-M-return>") 'indent-new-comment-line)

;; Shortcut to enable flyspell for buffer
(global-set-key "\C-cs" 'flyspell-enable)

;; Another keystroke alias for this command, which should work reliably in
;; the terminal.
(global-set-key "\C-c," 'flyspell-goto-next-error)

;; Always start a new ispell process for flyspell checking; to not do so is
;; unbearably slow with aspell on Windows.
(setq flyspell-large-region 0)

;; Recheck spelling after running ispell-key keyboard macro
;(global-set-key "\M-$" 'flyspell-word)

;; Global command for entering iimage-mode
(global-set-key "\C-ci" 'iimage-mode)

;; Don't nag about saving the personal dictionary every time we add a new
;; word with ispell
(setq ispell-silently-savep t)


;;; CUSTOM COMMANDS

(global-set-key "\C-cm" 'timestamp-insert)


;;; MINOR MODES

(setq git-blame-prefix-format "%h %28.28A:")


;;; CUSTOM MODE HOOKS AND SETTINGS

;; Inferior SLIME
(add-paredit-hook inferior-slime-mode)

;; Enable paredit in the minibuffer, but only for the command eval-expression
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (if (eql this-command 'eval-expression)
                (paredit-mode t))))

;; Outline mode...
(require 'outline)
(add-to-list 'auto-mode-alist '("\\.ol$" . outline-mode))

;; Org mode...
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-ca" 'org-agenda)
; Backtab in terminal emulators such as gnome-terminal, konsole, etc.:
(define-key org-mode-map "\M-[z" 'org-shifttab)
; Use C-ct as an alternative for C-cC-t, so that we don't have to use quite
; as many keystrokes with GNU Screen's escape bound to C-t
(define-key org-mode-map "\C-ct" 'org-todo)
(setq org-cycle-separator-lines 2
      org-special-ctrl-a/e t
      org-agenda-start-on-weekday 0
      org-completion-use-ido t
      org-agenda-window-setup 'current-window)
; Always show context when creating sparse trees:
(setq org-show-siblings t)

;; Calendar mode...
(global-set-key "\C-cl" 'calendar)
(add-hook 'calendar-mode-hook
          (lambda ()
            (make-local-variable 'scroll-margin)
            (setq scroll-margin 0)))

;; Diary mode...
(require 'diary-lib)

(defun diary-range (first &optional last on-sexps off-sexps)
  "Diary entry for a event spanning over a range of dates."
  (and
   (or (null first)
       (not (calendar-date-compare (list date) (list first))))
   (or (null last)
       (not (calendar-date-compare (list last) (list date))))
   (or (null on-sexps)
       (eql on-sexps t)
       (eval (cons 'or (mapcar #'(lambda (d)
                                   (not (null (eval d)))) on-sexps))))
   (or (null off-sexps)
       (not (eval (cons 'or (mapcar #'(lambda (d)
                                   (not (null (eval d)))) off-sexps)))))
   (cons diary-marking-entries-flag entry)))

(defun diary-weekdays (&rest args)
  (if (find (calendar-day-of-week date) args)
      (cons diary-marking-entries-flag entry)
    nil))

(global-set-key "\C-cd" 'diary)
(add-hook 'diary-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (make-local-variable 'tab-stop-list)
            (setq tab-stop-list '(4 16))
            (local-set-key (kbd "TAB") 'tab-to-tab-stop)))
(setq diary-display-function 'diary-fancy-display)
(add-hook 'list-diary-entries-hook 'include-other-diary-files)
(add-hook 'mark-diary-entries-hook 'mark-included-diary-files)

;; Calc mode...
(global-set-key "\C-cc" 'calc)

;; HTML mode...
(add-to-list 'auto-mode-alist '("\\.mtml$" . html-mode))
(add-hook 'html-mode-hook
          (lambda ()
            (auto-fill-mode 0)
            (setq indent-tabs-mode nil)
            (set (make-local-variable 'sgml-basic-offset) 2)
            (sgml-guess-indent)
            (local-set-key "\C-m" 'newline-and-indent)
            (local-set-key "\C-j" 'newline)))

;; Markdown mode...
(autoload 'markdown-mode
  "markdown-mode.el"
  "Major mode for editing Markdown files."
  t)
(setq auto-mode-alist
      (append '(("\\.mkd$" . markdown-mode)
                ("\\.markdown$" . markdown-mode))
              auto-mode-alist))
(add-hook 'markdown-mode-hook
          (lambda ()
            (auto-fill-mode t)))

;; YAML mode...
(autoload 'yaml-mode "yaml-mode.el")
(setq auto-mode-alist
      (append '(("\\.yaml$" . yaml-mode))
              auto-mode-alist))
(add-hook 'yaml-mode-hook
          (lambda ()
            (local-set-key "\C-cn" 'new-yaml-ab-entry)
            (make-local-variable 'tab-stop-list)
            (setq tab-stop-list (simple-tab-stop-list 2 75))))

;; C mode...
(add-hook 'c-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)))

;; ASM mode...
(setq asm-comment-char 35)
(add-hook 'asm-mode-hook
          (lambda ()
            (setq tab-width 3)
            (make-local-variable 'tab-stop-list)
            (setq tab-stop-list (simple-tab-stop-list 3 75))
            (setq indent-tabs-mode t)
            (local-set-key (kbd "TAB") 'tab-to-tab-stop)))

;; Perl mode...
(defalias 'perl-mode 'cperl-mode)
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm][wW]?\\|al\\|t\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
(setq cperl-close-paren-offset -4
      cperl-continued-statement-offset 4
      cperl-indent-level 4
      cperl-indent-parens-as-block t
      cperl-tab-always-indent t)
(add-hook 'cperl-mode-hook
          (lambda ()
            (set-fill-column 78)
            (setq cperl-indent-level 4)
            (setq cperl-continued-statement-offset 8)
            (abbrev-mode 0)
            (local-set-key "\C-cj" 'perl-doc-sub)
            (local-set-key "\C-cp" 'perl-doc-pod)))

;; Python mode...
(add-hook 'python-mode-hook
          (lambda ()
            (abbrev-mode 0)))

;; JavaScript mode...
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))

;; Emacs Lisp mode...
(add-paredit-hook emacs-lisp-mode)

;; Common Lisp mode...
(add-hook 'lisp-mode-hook
          (lambda ()
            (setq lisp-indent-function 'common-lisp-indent-function)))
(add-paredit-hook lisp-mode)

;; Clojure mode...
(when (featurep 'slime)
  (require 'swank-clojure nil t))
(add-paredit-hook clojure-mode)

;; Groovy mode...
(add-to-list 'auto-mode-alist '("\\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;; Haskell mode...
(load "haskell-site-file" t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;(if (featurep 'haskell-mode)
;    (setq haskell-indent-look-past-empty-line nil))

;; Visual Basic mode...
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|vbs\\|cls\\)$" .
                                visual-basic-mode)) auto-mode-alist))
(setq visual-basic-mode-indent 4)

;; PowerShell mode...
(autoload 'powershell-mode "powershell-mode" t)
(setq auto-mode-alist (append '(("\\.\\ps1$" . powershell-mode))
                              auto-mode-alist))

;; Text mode...
(define-key text-mode-map "\C-m" 'newline)
(define-key text-mode-map "\C-j" 'newline-and-indent)
(define-key text-mode-map "\C-cn" 'new-journal-entry)
(add-hook 'text-mode-hook
          (lambda ()
            (setq tab-width 8
                  indent-tabs-mode t)
            (auto-fill-mode 1)))

;; Paragraph indent text mode...
(add-to-list 'auto-mode-alist '("\\.txt$" . text-mode))

;; AUCTeX / LaTeX mode...
(load "auctex.el" t t t)
(load "preview-latex.el" t t t)
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (local-set-key "\C-m" 'newline-and-indent)
            (local-set-key "\C-j" 'newline)
            (setq tab-width 4)
            (auto-fill-mode 1)))

;; Mail mode...
(add-to-list 'auto-mode-alist '("mutt-.*-[0-9]+-[0-9]+-[0-9]+$" . mail-mode))
(add-hook 'mail-mode-hook
          (lambda ()
            (set-fill-column 72)
            (flyspell-enable)
            (end-of-buffer)
            (dotimes (num 4 nil)
              (previous-line))))

;; Shell mode...
(add-hook 'shell-mode-hook
          (lambda ()
            (make-local-variable 'scroll-margin)
            (setq scroll-margin 0)))


;;; CUSTOM EXTENDED COMMANDS

;; Show an ASCII table
;; (Inspired by http://www-cdf.fnal.gov/~sthrlnd/emacs_help.html)
(defun ascii-table ()
  "Show a table of ASCII characters by decimal, hex, and octal value."

  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (let ((min 1) (max 255)
        (special-chars '(
                         (1 . "%c  SOH (start of heading)")
                         (2 . "%c  STX (start of text)")
                         (3 . "%c  ETX (end of text)")
                         (4 . "%c  EOT (end of transmission)")
                         (5 . "%c  ENQ (enquiry)")
                         (6 . "%c  ACK (acknowledge)")
                         (7 . "%c  BEL (bell)")
                         (8 . "%c  BS  (backspace)")
                         (9 . "    TAB (horizontal tab)")
                         (10 . "    LF  (NL line feed, new line)")
                         (11 . "%c  VT  (vertical tab)")
                         (12 . "    FF  (NP form feed, new page)")
                         (13 . "%c  CR  (carriage return)")
                         (14 . "%c  SO  (shift out)")
                         (15 . "%c  SI  (shift in)")
                         (16 . "%c  DLE (data link escape)")
                         (17 . "%c  DC1 (device control 1)")
                         (18 . "%c  DC2 (device control 2)")
                         (19 . "%c  DC3 (device control 3)")
                         (20 . "%c  DC4 (device control 4)")
                         (21 . "%c  NAK (negative acknowledge)")
                         (22 . "%c  SYN (synchronous idle)")
                         (23 . "%c  ETB (end of trans. block)")
                         (24 . "%c  CAN (cancel)")
                         (25 . "%c  EM  (end of medium)")
                         (26 . "%c  SUB (substitute)")
                         (27 . "%c  ESC (escape)")
                         (28 . "%c  FS  (file separator)")
                         (29 . "%c  GS  (group separator)")
                         (30 . "%c  RS  (record separator)")
                         (31 . "%c  US  (unit separator)")
                         (32 . "%c       (space)")
                         (9999)
                         )))
    (insert (format "ASCII characters %d-%d\n\n" min max))
    (insert " Dec   Hex   Oct    Character\n")
    (let ((i 0))
      (while (< i 60)
        (insert "=")
        (setq i (+ i 1))))
    (insert "\n")
    (let ((i min))
      (while (<= i max)
        (let ((line "%4d  0x%02X  %04o    ") (char "%c"))
          (while (> i (car (car special-chars)))
            (setq special-chars (cdr special-chars)))
          (if (= (car (car special-chars)) i)
              (setq char (cdr (car special-chars))))
          (insert (format (concat line char "\n") i i i i))
          (setq i (+ i 1))))))
  (beginning-of-buffer))


(defun prompt-user-selection (choices &optional message)
  "Prompt user to make selection from given set of choices

The selection is specified as an assoc list of single-character
strings to choice descriptions, e.g.:

\(prompt-user-selection '((\"1\" . \"one\")
                         (\"2\" . \"two\"))\)

Choices are case-sensitive.  The function keeps prompting until
user makes a valid selection, or presses the quit keystroke.  The
return value is the chosen assoc list element; to get just the
user's choice, take the car of the return value.  An optional
message argument causes this function to display the given
message as a prompt above the list of choices.
"

  (let ((answer nil))
    (while (null answer)

      ;; Show a list of choices
      (unless (null message)
        (princ (concat message "\n\n") t))
      (dolist (option choices)
        (princ (concat (car option)
                       ": "
                       (cdr option)
                       "\n")
               t))

      ;; Wait for the user to make a selection
      (setq answer (assoc (char-to-string (read-char))
                          choices)))
    answer))


(defun perl-doc-sub ()
  "Insert standard comment for a Perl sub

Creates a comment for the current sub, if any, which follows the
basic format outlined in _Perl Best Practices_.
"

  (interactive)
  (move-end-of-line nil)
  (if (search-backward-regexp "^sub\s")
      (let* ((sub-choices '(("c" . "CLASS METHOD")
                            ("i" . "INSTANCE METHOD")
                            ("s" . "INTERFACE SUB")
                            ("u" . "INTERNAL UTILITY")))
             (sub-label (cdr (prompt-user-selection sub-choices
                                                    "Choose subroutine type:"))))
        (progn (insert "### " sub-label " ###\n"
                       "# Purpose    : \n"
                       "# Returns    : \n"
                       "# Parameters : \n"
                       "# Throws     : No exceptions\n"
                       "# Comments   : None\n"
                       "# See Also   : N/A\n")
               (previous-line 6)
               (move-end-of-line nil)))))


(defun perl-doc-pod ()
  "Insert a Perl POD documentation template for the current module"

  (interactive)
  (let* ((doc-choices '(("m" . "MODULE")
                        ("a" . "APPLICATION")))
         (doc-type (car (prompt-user-selection doc-choices
                                               "Choose POD documentation template:")))
         (doc-template-dir (concat user-emacs-directory "templates/documentation/"))
         (doc-template-file (cond
                             ((equal doc-type "m") "perl-pod-module")
                             ((equal doc-type "a") "perl-pod-application")))
         (lic-choices '(("n" . "NONE")
                        ("p" . "PERL ARTISTIC")
                        ("a" . "APACHE")))
         (lic-type (car (prompt-user-selection lic-choices
                                               "Choose a software license:")))
         (lic-template-dir (concat user-emacs-directory "templates/license/"))
         (lic-template-file (cond
                             ((equal lic-type "p") "perl-artistic")
                             ((equal lic-type "a") "apache"))))
    (end-of-buffer)
    (insert-file-contents (concat doc-template-dir doc-template-file))
    (if lic-template-file
        (progn
          (end-of-buffer)
          (insert-file-contents (concat lic-template-dir lic-template-file))))))


(defun calendar-zone-to-tz-offset (minutes)
  "Converts minutes off from UTC into a TZ offset string

Converts from a number of minutes off from UTC (as in the
calendar-time-zone variable) to a timezone specification in the
format returned by (format-time-string \"%z\" now).
"

  (let ((sign ""))
    (when (< minutes 0)
      (setq minutes (* minutes -1)
            sign    "-"))
    (concat sign (format "%02d%02d" (floor minutes 60) (mod minutes 60)))))


(defun timestamp-string ()
  "Returns a Unix date(1)-format timestamp

Will try figure out the timezone name from your
calendar-standard-time-zone-name and
calendar-daylight-time-zone-name variables if the system doesn't
return a time zone name -- as with NT Emacs as of version 23.2,
for example.
"

  (let* ((now (current-time))
         (str-date (format-time-string "%a %b %e" now))
         (str-time (format-time-string "%H:%M:%S" now))
         (sys-tz   (format-time-string "%Z" now))
         (str-year (format-time-string "%Y" now))
         (str-tz (if (> (length sys-tz) 0)
                     sys-tz
                     (let ((off-tz (format-time-string "%z" now)))
                       (cond ((equal off-tz
                                     (calendar-zone-to-tz-offset
                                      calendar-time-zone))
                              calendar-standard-time-zone-name)

                             ((equal off-tz
                                     (calendar-zone-to-tz-offset
                                      (+ 60 calendar-time-zone)))
                              calendar-daylight-time-zone-name)

                             (t
                              nil))))))
    (if str-tz
        (concat str-date " " str-time " " str-tz " " str-year)
        (concat str-date " " str-time " " str-year))))


(defun timestamp-insert ()
  "Inserts a Unix date(1)-format timestamp in the current buffer"

  (interactive)
  (insert (timestamp-string)))


;; Create a new journal entry
(defun new-journal-entry ()
  "Make a new journal entry with a Unix `date`-style timestamp"

  (interactive)
  (end-of-buffer)
  (if (re-search-backward "[^ \t\n]" nil t)
      (progn
        (end-of-line)
        (let ((beg (point)))
          (end-of-buffer)
          (delete-region beg (point)))
        (dotimes (i 3)
          (newline))))
  (insert (timestamp-string))
  (dotimes (i 2)
    (newline)))


;; New YAML address book item
(defun new-yaml-ab-entry ()
  "Make a new entry in a YAML address book"

  (interactive)

  (if (re-search-backward "^\\\.\\\.\\\.$" nil t)
      (next-line)
    (beginning-of-buffer))
  (insert "---")
  (newline)

  (insert "name:")
  (newline)
  (tab-to-tab-stop)
  (insert "family: ")
  (newline)
  (tab-to-tab-stop)
  (insert "given: ")
  (newline)

  (insert "email: ")
  (newline)

  (insert "telephone:")
  (newline)
  (tab-to-tab-stop)
  (insert "mobile: ")
  (newline)

  (insert "address:")
  (newline)
  (tab-to-tab-stop)
  (insert "street: ")
  (newline)
  (tab-to-tab-stop)
  (insert "city: ")
  (newline)
  (tab-to-tab-stop)
  (insert "state: ")
  (newline)
  (tab-to-tab-stop)
  (insert "zip: ")
  (newline)

  (insert "...")
  (newline)

  (re-search-backward "^---$")
  (dotimes (i 2)
    (next-line))
  (end-of-line))


;; Consolidate flyspell commands
(defun flyspell-enable ()
  "Enable flyspell for the current buffer"

  (interactive)

  (flyspell-buffer)
  (flyspell-mode 1))

(defun flyspell-word ()
  "Check the current word's spelling and then re-run flyspell"

  (interactive)

  (command-execute 'ispell-word)
  (flyspell-buffer))


;; Custom variables from the Emacs configuration editor
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((TeX-master . "manual") (TeX-master . t)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )


