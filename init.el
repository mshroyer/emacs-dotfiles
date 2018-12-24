;;; init.el --- mshroyer's emacs init

;; Intended for use with Emacs 25 or newer.

;;;; Language dependencies.


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cl-lib)

;;;; Operating system support.

;;; MacOS.

(when (eq system-type 'darwin)
  ;; Use the Option/Alt key for Meta in Emacs.app
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'meta)

  ;; Rig up cmd-H so that it hides the application window, rather than mark
  ;; the current paragraph...
  (global-set-key "\M-h" 'ns-do-hide-emacs)

  ;; As of version 25.3, Emacs doesn't seem able to find the root
  ;; certificate store on MacOS on its own.  This is necessary in order to
  ;; load packages over HTTPS.
  (require 'gnutls)
  (add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))

;;; Windows.

;; The HOME environment variable may not necessarily be set on Windows
;; systems.  If it isn't already set, try to synthesize it from other
;; standard Windows environment variables.
(when (and (member system-type '(windows-nt msdos))
           (not (getenv "HOME")))
  (setenv "HOME" "$HOMEDRIVE$HOMEPATH" t))

;;;; Dependencies.

;;; Load paths and package sources.

;; User Emacs directories
(setq user-emacs-directory "~/.emacs.d"
      user-elisp-directory (concat user-emacs-directory "/elisp")
      generated-autoload-file (concat user-emacs-directory
				      "/loaddefs.el"))

;; Functions defined in mshroyer-lib are required for constructing the full
;; set of load paths, so we must explicitly load it first.
(require 'mshroyer-lib (concat user-elisp-directory "/mshroyer-lib.el"))

;;;; Local settings.

(defvar local-frame-font "Source Code Pro-10"
  "Font to be used for GUI frames.")

(defvar local-server-selection '(:emacs)
  "Server types to be launched at emacs startup.")

;; Retrieve any local configurations from ~/.emacs.local.el, if the file
;; exists on this system.
(let ((local-settings "~/.emacs.local.el"))
  (if (file-exists-p local-settings)
      (load-file local-settings)))

(defun local-frame-setup (frame)
  "Apply local settings to a frame."
  (with-selected-frame frame
    (when (and frame (assoc 'font-backend (frame-parameters)))
      (set-frame-font local-frame-font nil t))))

(mshroyer-add-frame-hook #'local-frame-setup)

;;;; Custom variables.

;; Custom variables from the Emacs configuration editor
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-verbatim-environments (quote ("verbatim" "verbatim*" "Verbatim")))
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(compilation-error-regexp-alist
   (quote
    (google3-build-log-parser-info google3-build-log-parser-warning google3-build-log-parser-error google3-build-log-parser-python-traceback google-blaze-error google-log-error google-log-warning google-log-info google-log-fatal-message google-forge-python gunit-stack-trace absoft ada aix ant bash borland python-tracebacks-and-caml cmake cmake-info comma cucumber msft edg-1 edg-2 epc ftnchek iar ibm irix java jikes-file maven jikes-line clang-include clang-include gcc-include ruby-Test::Unit gnu lcc makepp mips-1 mips-2 msft omake oracle perl php rxp sparc-pascal-file sparc-pascal-line sparc-pascal-example sun sun-ada watcom 4bsd gcov-file gcov-header gcov-nomark gcov-called-line gcov-never-called perl--Pod::Checker perl--Test perl--Test2 perl--Test::Harness weblint guile-file guile-line)))
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("b8c5adfc0230bd8e8d73450c2cd4044ad7ba1d24458e37b6dec65607fc392980" default)))
 '(ecb-layout-window-sizes nil)
 '(ecb-options-version "2.40")
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults)))
 '(global-mark-ring-max 8)
 '(gnus-thread-sort-functions (quote (gnus-thread-sort-by-most-recent-date)))
 '(gnus-treat-display-smileys nil)
 '(gofmt-command "~/go/bin/goimports")
 '(helm-split-window-inside-p t)
 '(mark-ring-max 8)
 '(package-selected-packages
   (quote
    (helm-org-rifle auctex go-mode elpy markdown-mode web-mode lua-mode nasm-mode expand-region undo-tree magit helm diminish use-package cider)))
 '(safe-local-variable-values (quote ((TeX-master . "manual") (TeX-master . t))))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-even-diff-A ((t (:background "dim gray"))))
 '(ediff-even-diff-Ancestor ((t (:background "dim gray"))))
 '(ediff-even-diff-B ((t (:background "dim gray"))))
 '(ediff-even-diff-C ((t (:background "dim gray"))))
 '(ediff-odd-diff-A ((t (:background "dim gray"))))
 '(ediff-odd-diff-B ((t (:background "dim gray"))))
 '(ediff-odd-diff-C ((t (:background "dim gray"))))
 '(fixed-pitch ((t (:inherit nil))))
 '(isearch ((t (:background "pale violet red" :foreground "white"))))
 '(markdown-code-face ((t (:inherit fixed-pitch))))
 '(org-level-1 ((t (:inherit outline-1 :foreground "yellow green"))))
 '(org-level-2 ((t (:inherit outline-2 :foreground "LightGoldenrod1"))))
 '(trailing-whitespace ((t (:background "khaki3" :foreground "black")))))

;;;; Packages.

(setf elisp-tree `((,user-elisp-directory)))

;; Prepend user elisp directories to the elisp load path.  Then, prepare
;; any autoloads contained in our user load paths.
(let ((my-load-path (cl-remove-if-not #'file-exists-p
                                      (mshroyer-flatten-path-tree elisp-tree))))
  (setq load-path (append my-load-path load-path))
  (apply #'update-directory-autoloads my-load-path))

;; Contains autoloads processed from the user-elisp tree.
(load generated-autoload-file)

(load-file (concat user-emacs-directory "/init-packages.el"))

;;; Required features.

(require 'mshroyer-lib)
(require 'org)
(require 'calendar)
(require 'diary-lib)
(require 'paredit)
(require 'tramp)
(require 'google-c-style)
(require 'sudoku)

;;; UTILITY FUNCTIONS

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

(defun zero-scroll-margin ()
  "Zeros the scroll margin in the current buffer"
  (make-local-variable 'scroll-margin)
  (setq scroll-margin 0))


;;; FILE HANDLING

;; Auto-save every four minutes, and delete auto-save files when the buffer
;; is saved or killed
(setq auto-save-interval 240
      delete-auto-save-files t)

;; Like Vim's ":set backupcopy=yes"; save files in-place so things like
;; Mutt or Cygwin's permissions don't get screwed up.
(setq make-backup-files t
      backup-by-copying t)

;; Backup file settings
(setq version-control t
      kept-new-versions 6
      kept-old-versions 2
      delete-old-versions t)

;; Always store backup and auto-save files under tmp
(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "/backups")))

      auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "/autosaves") t)))

;; Make sure the last line of a file ends in a carriage return
(setq require-final-newline t)

;; Find file at point
(global-set-key "\C-cof" 'find-file-at-point)

;; Other file
(global-set-key "\C-coo" 'ff-find-other-file)


;;; GENERAL INTERFACE SETTINGS

;; Don't display startup message
(setq inhibit-startup-message t)

;; I don't like bells.
(setq ring-bell-function 'ignore)

;; Get rid of the toolbar, if applicable
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))

;; Enable menu-bar-mode, but hide the menu bar on non-graphical frames.
(defun conditionally-set-menu-bar-lines (frame)
  "Set menu-bar-lines depending on whether the frame is graphical."
  (let ((menu-bar-lines (if (display-graphic-p frame) 1 0)))
    (set-frame-parameter frame 'menu-bar-lines menu-bar-lines)))

(mshroyer-add-frame-hook #'conditionally-set-menu-bar-lines)

;; Use C-x w [pnfb] to navigate directionally between windows
(global-set-key "\C-cwp" 'windmove-up)
(global-set-key "\C-cwn" 'windmove-down)
(global-set-key "\C-cwb" 'windmove-left)
(global-set-key "\C-cwf" 'windmove-right)

;; Also allow C-x w C-[pnfb] because this feels a little more like my tmux config
(global-set-key "\C-cw\C-p" 'windmove-up)
(global-set-key "\C-cw\C-n" 'windmove-down)
(global-set-key "\C-cw\C-b" 'windmove-left)
(global-set-key "\C-cw\C-f" 'windmove-right)

;; Specify Unix system EOL mnemonics (these settings won't be the default
;; on Windows versions of Emacs)
(setq eol-mnemonic-unix ":"
      eol-mnemonic-dos  "(DOS)"
      eol-mnemonic-mac  "(MAC)")

;; Display current time and load average on mode line
(display-time)

;; Enable paredit in the minibuffer, but only for the command eval-expression
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (if (eql this-command 'eval-expression)
                (paredit-mode t))))

;; Show column number on the mode line
(column-number-mode t)

;; Use visual bell
(setq visible-bell t)

;; Show continuation lines
(setq truncate-lines nil
      truncate-partial-width-windows nil)

;; Don't make me type out long answers...
(fset 'yes-or-no-p 'y-or-n-p)

;; Scroll one line at a time, like Vim
;(setq scroll-conservatively 2)

;; Show at least three lines of context around the cursor while scrolling
;; (like :set scrolloff=3 in Vim)
(setq scroll-margin 3)

;; Custom key bindings for line scrolling without moving point
(global-set-key (kbd "C-.") #'scroll-up-line)
(global-set-key (kbd "C-,") #'scroll-down-line)

;; Allow repeated pops from the mark ring with C-u C-SPC C-SPC ...
(setq set-mark-command-repeat-pop t)

;; Don't mess around with this disabled commands nonsense
(setq disabled-command-hook nil)

;; Confirm that we want to quit Emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Add a keystroke for renaming a buffer
(global-set-key "\C-cr" 'rename-buffer)

;; Don't limit expression evaluation output
(setq eval-expression-print-length nil
      eval-expression-print-level  nil)

(global-set-key "\C-ce" 'mshroyer-toggle-current-window-dedication)


;;; EDITING OPTIONS

;; Text mode abbreviations
(setq-default abbrev-mode t)
(setq abbrev-file-name (concat user-emacs-directory "/abbrev_defs")
      save-abbrevs     t)

;; Quick entry for commonly used symbols
(defconst specialchar-en-dash #x2013)
(defconst specialchar-em-dash #x2014)
(global-set-key "\C-c-" (lambda (&optional arg)
                          (interactive "*P")
                          (if arg
                              (ucs-insert specialchar-em-dash)
                            (ucs-insert specialchar-en-dash))))

;; Set up syntax coloring
(global-font-lock-mode 1)

;; Turn on paren matching (this is a Lisp editor, is it not?)
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; Use spaces for indentation, not tab chracters
(setq-default indent-tabs-mode nil
              tab-width 8)
(setq standard-indent 4
      c-indent-level 4)

;; Use tab key for completion too
(setq tab-always-indent 'complete)

;; Always use auto-fill in text mode; wrap to 75 characters by default
(setq-default fill-column 75)

;; Show trailing whitespace
(setq show-trailing-whitespace t)

;; Shortcut to enable flyspell for buffer
(global-set-key "\C-cs" 'mshroyer-flyspell-enable)

;; Another keystroke alias for this command, which should work reliably in
;; the terminal.
(global-set-key "\C-c," 'flyspell-goto-next-error)

;; Always start a new ispell process for flyspell checking; to not do so is
;; unbearably slow with aspell on Windows.
(setq flyspell-large-region 0)

;; Global command for entering iimage-mode
(global-set-key "\C-ci" 'iimage-mode)

;; Don't nag about saving the personal dictionary every time we add a new
;; word with ispell
(setq ispell-silently-savep t)

;; Don't spell check HTML/XML tags and attributes
(set-default 'ispell-skip-html t)

;; Blame mode formatting
(setq git-blame-prefix-format "%h %28.28A:")

;; Auto complete options
(when (featurep 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories (concat user-elisp-directory
                                                  "/auto-complete/dict"))
  (ac-config-default)

  (setq ac-auto-start nil)
  (ac-set-trigger-key "TAB"))

;; Default compile command-I don't like make -k
(setq-default compile-command "make ")


;;; CUSTOM COMMANDS

(global-set-key "\C-cu" 'calc)
(global-set-key "\C-cm" 'mshroyer-insert-timestamp)
(global-set-key "\C-cg" 'mshroyer-create-tags)
(global-set-key "\C-cp" 'compile)
(global-set-key "\C-cf" 'auto-fill-mode)


;;; EDIFF

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;; PAREDIT

(add-hook 'paredit-mode-hook
          (lambda ()
            (local-set-key (kbd "M-[") 'paredit-wrap-square)))


;;; TRAMP

;; Assume SCP if no explicit method
(setq tramp-default-method "scp")


;;; GDB

;; Use global keybindings for the window selection functions so that they
;; work from the source window too...
(mapcar (lambda (setting)
          (lexical-let ((key    (car setting))
                        (header (cdr setting)))
            (global-set-key (concat "\C-c\C-g" key)
                            (lambda ()
                              (interactive)
                              (mshroyer-gdb-select-window header)))))
        '(("c" . comint)
          ("l" . locals)
          ("r" . registers)
          ("u" . source)
          ("s" . stack)
          ("b" . breakpoints)
          ("t" . threads)))


;;; ORG MODE / DIARY

(load-file (concat user-emacs-directory "/init-org.el"))
(load-file (concat user-emacs-directory "/init-diary.el"))

;; Override org-mode keybindings so M-h still hides emacs on macOS.
(when (eq system-type 'darwin)
  (add-hook 'org-mode-hook
            (lambda ()
              (local-set-key "\M-h" 'ns-do-hide-emacs))))

(setf org-checkbox-hierarchical-statistics nil)


;;; ERC

(load-file (concat user-emacs-directory "/init-erc.el"))


;;; EDITING MODE HOOKS AND SETTINGS

;; Help mode...
(add-hook 'help-mode-hook
  (lambda ()
    (define-key help-mode-map "l" 'help-go-back)))

;; Calendar mode...
(global-set-key "\C-cl" 'calendar)
(add-hook 'calendar-mode-hook
          (lambda ()
            (make-local-variable 'scroll-margin)
            (setq scroll-margin 0)))

;; HTML mode...
(let ((mode (if (featurep 'web-mode) 'web-mode 'html-mode)))
  (add-to-list 'auto-mode-alist (cons "\\.mtml$" mode))
  (add-to-list 'auto-mode-alist (cons "\\.ng$" mode))
  (add-to-list 'auto-mode-alist (cons "\\.soy$" mode))
  (add-to-list 'auto-mode-alist (cons "\\.html?\\'" mode)))

;; CSS mode...
(add-to-list 'auto-mode-alist '("\\.gss$" . css-mode))

;; Markdown mode...
(when (featurep 'markdown-mode)
  (setq auto-mode-alist
        (append '(("\\.mk?d$" . markdown-mode)
                  ("\\.markdown$" . markdown-mode))
                auto-mode-alist))
  (add-hook 'markdown-mode-hook
          (lambda ()
            (auto-fill-mode t))))

;; YAML mode...
(when (featurep 'yaml-mode)
  (setq auto-mode-alist
        (append '(("\\.yaml$" . yaml-mode))
                auto-mode-alist))
  (add-hook 'yaml-mode-hook
            (lambda ()
              (local-set-key "\C-cn" 'new-yaml-ab-entry)
              (make-local-variable 'tab-stop-list))))

;; C mode...
(setq-default c-block-comment-prefix "* ")
(when (featurep 'smart-tabs-mode)
  (smart-tabs-insinuate 'c))
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))
(c-add-style
 "linux-tabs-only"
 '("linux" (c-offsets-alist
            (arglist-cont-nonempty
             c-lineup-gcc-asm-reg
             c-lineup-arglist-tabs-only))))
(c-add-style
 "ilchymis"
 '("gnu"
   (c-offsets-alist . ((arglist-intro . +)
                       (arglist-cont  . 0)
                       (arglist-close . 0)))
   (c-basic-offset  . 4)))
(c-add-style "my-c++" 
	     '("stroustrup"
	       (indent-tabs-mode . nil)        ; use spaces rather than tabs
	       (c-basic-offset . 4)            ; indent by four spaces
	       (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
				   (brace-list-open . 0)
				   (statement-case-open . +)
                                   (innamespace . 0)))
               (c-hanging-braces-alist . ((namespace-open after)))))
(setq c-default-style "linux")
(add-hook 'c-initialization-hook
          (lambda ()
            (define-key c-mode-base-map "\C-m" 'c-context-line-break)))
(add-hook 'c-mode-hook
          (lambda ()
            (make-local-variable 'paragraph-start)
            (setq paragraph-start
                  "^[ ]*\\(//+\\|\\**\\)[ ]*\\([ ]*$\\|@[a-zA-Z].*\\)\\|^\f")
            (setq show-trailing-whitespace t)
            (semantic-mode t)))

;; C++ mode...

;; Taken from: https://gist.github.com/benzap/2895437#file-emacs-el-L338
;;fix issues with c++11 variables not being understood
(defun --copy-face (new-face face)
  "Define NEW-FACE from existing FACE."
  (copy-face face new-face)
  (eval `(defvar ,new-face nil))
  (set new-face new-face))
(--copy-face 'font-lock-label-face  ; labels, case, public, private, proteced, namespace-tags
             'font-lock-keyword-face)
(--copy-face 'font-lock-doc-markup-face ; comment markups such as Javadoc-tags
             'font-lock-doc-face)
(--copy-face 'font-lock-doc-string-face ; comment markups
             'font-lock-comment-face)
(add-hook 'c++-mode-hook
          '(lambda()
             (font-lock-add-keywords
              nil '(;; complete some fundamental keywords
                    ("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" . font-lock-keyword-face)
                    ;; add the new C++11 keywords
                    ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
                    ("\\<\\(char[0-9]+_t\\)\\>" . font-lock-keyword-face)
                    ;; PREPROCESSOR_CONSTANT
                    ("\\<[A-Z]+[A-Z_]+\\>" . font-lock-constant-face)
                    ;; hexadecimal numbers
                    ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
                    ;; integer/float/scientific numbers
                    ("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>" . font-lock-constant-face)
                    ;; user-types (customize!)
                    ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(t\\|type\\|ptr\\)\\>" . font-lock-type-face)
                    ("\\<\\(xstring\\|xchar\\)\\>" . font-lock-type-face)
                    ))
             ) t)

;; Fix enum class indentation
;; Taken from here, with slight modification:
;; http://stackoverflow.com/questions/6497374/emacs-cc-mode-indentation-problem-with-c0x-enum-class
(defun inside-class-enum-p (pos)
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "enum[ \t]+class[ \t]+"))))

(defun align-enum-class (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      0
    (c-lineup-topmost-intro-cont langelem)))

(defun align-enum-class-closing-brace (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      '-
    '+))

(defun fix-enum-class ()
  "Setup `c++-mode' to better handle \"class enum\"."
  (add-to-list 'c-offsets-alist '(topmost-intro-cont . align-enum-class))
  (add-to-list 'c-offsets-alist
               '(statement-cont . align-enum-class-closing-brace)))

(add-hook 'c++-mode-hook 'fix-enum-class)

(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-style "my-c++")))

;; Java mode...
(add-hook 'java-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)
            (set-fill-column 100)))

;; GUD mode...
(add-hook 'gud-mode-hook
          (lambda ()
            (make-local-variable 'scroll-margin)
            (setq scroll-margin 0)))

;; Go mode...
(when (featurep 'go-mode)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook
            (lambda ()
              (flyspell-prog-mode))))

;; Python mode...
(add-hook 'python-mode-hook
          (lambda ()
            (abbrev-mode 0)
            (setq show-trailing-whitespace t)
            (local-set-key (kbd "C-c b") #'python-shell-send-buffer)))
(add-hook 'inferior-python-mode-hook
          (lambda ()
            (make-local-variable 'scroll-margin)
            (setq scroll-margin         0
                  comint-process-echoes t)))
(when (featurep 'python)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter"))

;; Sh mode...
(setf sh-basic-offset 8)
(add-hook 'sh-mode-hook
          (lambda ()
            (setf indent-tabs-mode t)))

;; Lisp...
(setq slime-contribs '(slime-fancy))
(add-paredit-hook slime-repl-mode)
(add-paredit-hook lisp-mode)

;; JavaScript mode...
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))

;; Emacs Lisp mode...
(add-paredit-hook emacs-lisp-mode)

; Taken from http://paste.lisp.org/display/120878:
;
; When `paredit-mode' is enabled it takes precedence over the major
; mode effectively rebinding C-j to `paredit-newline' instead of
; `eval-print-last-sexp'.  I do not want this overridden in
; lisp-interaction-mode.  So, use the buffer-local
; `minor-mode-overriding-map-alist' to remove the C-j mapping from
; the standard `paredit-mode' bindings.
(add-hook 'lisp-interaction-mode-hook
	  (lambda ()
	    (paredit-mode 1)
	    (show-paren-mode 1)
	    (setq minor-mode-overriding-map-alist
		  `((paredit-mode
		     ,@(remove (cons ?\C-j 'paredit-newline)
			       paredit-mode-map))))))

;; Haskell mode...
(load "haskell-site-file" t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;(if (featurep 'haskell-mode)
;    (setq haskell-indent-look-past-empty-line nil))
(add-hook 'inferior-haskell-mode-hook
          (lambda ()
            (make-local-variable 'scroll-margin)
            (setq scroll-margin 0)))

;; Tuareg mode...
(setq auto-mode-alist (append '(("\\.ml[iylp]?$" . tuareg-mode))
                              auto-mode-alist))
(add-hook 'tuareg-interactive-mode-hook
          (lambda ()
            (make-local-variable 'scroll-margin)
            (setq scroll-margin 0)))

;; Text mode...
(define-key text-mode-map "\C-cn" 'mshroyer-new-journal-entry)
(define-key text-mode-map "\C-c\C-o" 'org-open-at-point)
(define-key text-mode-map "\C-ct" 'artist-mode)
(define-key text-mode-map (kbd "TAB") 'tab-to-tab-stop)
(add-hook 'text-mode-hook
          (lambda ()
            (make-local-variable 'tab-stop-list)
            (setq tab-width 8
                  tab-stop-list '(8 16 24 32 40 48 56 64 72 80 88 96 104 112 120)
                  indent-tabs-mode t)
            (auto-fill-mode 1)))

;; Paragraph indent text mode...
(add-to-list 'auto-mode-alist '("\\.txt$" . text-mode))

;; AUCTeX / LaTeX mode...
(when (featurep 'tex-site)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq tab-width        8
                    indent-tabs-mode nil)
              (auto-fill-mode 1))))

;; Mail mode...
(add-to-list 'auto-mode-alist '("mutt-.*" . mail-mode))
(add-hook 'mail-mode-hook
          (lambda ()
            (set-fill-column 72)
            (mshroyer-flyspell-enable)
            (search-forward "-- ")
            (previous-line)
            (open-line 1)))

;; Gnus
(load-file (concat user-emacs-directory "/init-gnus.el"))

;; Shell and Term mode...
(add-hook 'shell-mode-hook #'zero-scroll-margin)
(add-hook 'term-mode-hook #'zero-scroll-margin)


;;; EDITOR SERVERS

;; Check local-server-selection variable (possibly set in ~/.emacs.local.el)
;; to decide whether we should run the server in question
(defun should-start-server (server-name)
  (and (boundp 'local-server-selection)
       (member server-name local-server-selection)))

;; Builtin Emacs server
(when (should-start-server :emacs)
  (server-start)

  ;; Open files for "emacsclient" in a new frame...
  (add-hook 'server-switch-hook
            (lambda ()
              (let ((server-buf (current-buffer)))
                (bury-buffer)
                (switch-to-buffer-other-frame server-buf))))

  ;; ...and clean up when we're done with the client.
  (add-hook 'server-done-hook (lambda ()
                                (kill-buffer nil)
                                (redraw-display))))


;;; RESTORE DESKTOP

;(desktop-save-mode 1)
