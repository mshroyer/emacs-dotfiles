;;; -*- Mode: Emacs-Lisp; -*-
;;;
;;; My global configuration for GNU Emacs 23 and newer.
;;;
;;; Mark Shroyer
;;; https://markshroyer.com/

(require 'cl)

;;; LOCAL SETTINGS

;; Retrieve any local configurations from ~/.emacs.local.el, if the file
;; exists on this system
(let ((local-settings "~/.emacs.local.el"))
  (if (file-exists-p local-settings)
      (load-file local-settings)))


;;; UTILITY FUNCTIONS

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

;; Check local-server-selection variable (possibly set in ~/.emacs.local.el)
;; to decide whether we should run the server in question
(defun should-start-server (server-name)
  (and (boundp 'local-server-selection)
       (member server-name local-server-selection)))

(defun zero-scroll-margin ()
  "Zeros the scroll margin in the current buffer"
  (make-local-variable 'scroll-margin)
  (setq scroll-margin 0))


;;; SYSTEM

;; The HOME environment variable may not necessarily be set on Windows
;; systems.  If it isn't already set, try to synthesize it from other
;; standard Windows environment variables.
(when (and (member system-type '(windows-nt msdos))
           (not (getenv "HOME")))
  (setenv "HOME" "$HOMEDRIVE$HOMEPATH" t))

;; User Emacs directories
(setq user-emacs-directory "~/.emacs.d/"
      user-elisp-directory (concat user-emacs-directory "elisp/")
      generated-autoload-file (concat user-emacs-directory
				      "loaddefs.el"))

;; TLS program defaults that should result in validated server connections
;; and, in the case of GnuTLS, certificate pinning, given sufficiently
;; recent software versions.  These default settings should be verified on
;; each system where used and customized if necessary.
(setq tls-program '("gnutls-cli --strict-tofu -p %p %h"
                    "openssl s_client -no_ssl2 -verify 0 -verify_return_erro -connect %h:%p"))

;; Package archives
(when (< emacs-major-version 24)
  (load (concat user-elisp-directory "polyfill/package.el")))
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")))

;; Don't check for package signatures, since Marmalade does not currently
;; support signed packages.  We're already protected against MITM because
;; we access both Marmalade and ELPA over HTTPS.
(setq package-check-signature nil)
(package-initialize)

;; Tree(s) of paths containing submodule Emacs Lisp files.
(setq submodules-elisp `((,(concat user-emacs-directory "submodules/")
                          ("dash")
                          ("expand-region")
                          ("emacs-async")
                          ("helm")
			  ("with-editor")
                          ("magit" ("lisp"))
                          ("nasm-mode")
                          ("undo-tree")
                          ("web-mode")
                          ("lua-mode")
                          ("slime"))))

;; Prepend user elisp directories to the elisp load path.  Then, prepare
;; any autoloads contained in our user load paths.
(let ((my-load-path (remove-if-not #'file-exists-p
                                   (cons user-elisp-directory
					 (flatten-path-tree submodules-elisp)))))
  (setq load-path (append my-load-path load-path))
  (apply #'update-directory-autoloads my-load-path))

;; Add backported cl-lib to the load-path if we aren't on at least Emacs 24.3
(when (or (< emacs-major-version 24)
          (and (= emacs-major-version 24)
               (< emacs-minor-version 3)))
  (add-to-list 'load-path (concat user-elisp-directory "/polyfill/cl-lib"))
  (require 'cl-lib))


;;; EMACS EXTENSIONS

;; Contains autoloads processed from the user-elisp tree.
(load generated-autoload-file)

;; Required features
(require 'org)
(require 'calendar)
(require 'diary-lib)
(require 'paredit)
(require 'tramp)
(require 'google-c-style)
(require 'sudoku)
(require 'expand-region)
(require 'async)
(require 'helm)
(require 'helm-config)
(require 'nasm-mode)
(require 'web-mode)
(require 'magit)
(require 'undo-tree)
(require 'lua-mode)
(require 'slime)

;; Optional features

;; Must be set before Evil is required, so that the tab key doesn't break
;; in Org Mode.  See:
;; http://stackoverflow.com/questions/22878668/emacs-org-mode-evil-mode-tab-key-not-working
(setq evil-want-C-i-jump nil)

(require 'evil nil t)
(require 'color-theme nil t)


;;; MAC OS X-SPECIFIC CONFIGURATIONS

(when (eq system-type 'darwin)
  ;; Use the Option/Alt key for Meta in Emacs.app
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'meta)

  ;; Rig up cmd-H so that it hides the application window, rather than mark
  ;; the current paragraph...
  (global-set-key "\M-h" 'ns-do-hide-emacs))


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
      `((".*" . ,(concat user-emacs-directory "backups")))

      auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "autosaves/") t)))

;; Make sure the last line of a file ends in a carriage return
(setq require-final-newline t)

;; Find file at point
(global-set-key "\C-cof" 'find-file-at-point)

;; Other file
(global-set-key "\C-coo" 'ff-find-other-file)

;; ido mode for switching buffers and finding files
(setq ido-enable-flex-matching t
      ido-max-directory-size 1000000
      ido-auto-merge-work-directories-length -1
      ido-default-file-method 'selected-window
      ido-default-buffer-method ido-default-file-method)


;;; Magit

(global-set-key "\C-ct" 'magit-status)

;;; HELM

(global-set-key "\C-ch" 'helm-command-prefix)
(helm-mode 1)
(global-set-key "\C-xb" 'helm-mini)
(global-set-key "\C-x\C-f" 'helm-find-files)

;; Automatically scale helm window to fit its contents.
(helm-autoresize-mode t)

;; Increase the width of the buffer name column.
(setq helm-buffer-max-length 32)

;; Don't use helm for M-x.
(add-to-list 'helm-completing-read-handlers-alist '(execute-extended-command . nil))
(add-to-list 'helm-completing-read-handlers-alist '(ff-find-other-file . nil))
(add-to-list 'helm-completing-read-handlers-alist '(woman . nil))

;; Additional bindings.
(global-set-key "\M-y" 'helm-show-kill-ring)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t)

;; Show helm at the bottom of the frame
(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm" (* not-newline) "*" eos)
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.4)))


;;; GENERAL INTERFACE SETTINGS

;; Don't display startup message
(setq inhibit-startup-message t)

;; Get rid of the toolbar, if applicable
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))

;; By default, if we aren't running in a window system, turn off the menu
;; bar.
(when (not window-system)
  (menu-bar-mode 0))

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
(global-set-key (kbd "C-.") #'mshroyer/scroll-up)
(global-set-key (kbd "C-,") #'mshroyer/scroll-down)

;; Allow repeated pops from the mark ring with C-u C-SPC C-SPC ...
(setq set-mark-command-repeat-pop t)

;; Don't mess around with this disabled commands nonsense
(setq disabled-command-hook nil)

;; Confirm that we want to quit Emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Add a keystroke for renaming a buffer
(global-set-key "\C-cr" 'rename-buffer)

;; Don't use graphical confirmation dialogs; these cause a number of
;; problems, including hanging Emacs when we try to perform a system
;; shutdown in OS X.
;;
;; Borrowed from:
;; http://superuser.com/questions/125569/how-to-fix-emacs-popup-dialogs-on-mac-os-x
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

;; Don't limit expression evaluation output
(setq eval-expression-print-length nil
      eval-expression-print-level  nil)

;; Toggle the current window's dedication state -- taken from
;; http://dfan.org/blog/2009/02/19/emacs-dedicated-windows/
(defun toggle-current-window-dedication ()
  (interactive)
  (let* ((window    (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

(global-set-key "\C-ce" 'toggle-current-window-dedication)


;;; EDITING OPTIONS

;; Use undo-tree
(global-undo-tree-mode 1)

;; Text mode abbreviations
(setq-default abbrev-mode t)
(setq abbrev-file-name (concat user-emacs-directory "abbrev_defs")
      save-abbrevs     t)

;; expand-region
(global-set-key "\C-c=" 'er/expand-region)

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
(global-set-key "\C-cs" 'flyspell-enable)

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
                                                  "auto-complete/dict"))
  (ac-config-default)

  (setq ac-auto-start nil)
  (ac-set-trigger-key "TAB"))

;; Yasnippet options
(when (featurep 'yasnippet)
  (setq yas/prompt-functions '(yas/dropdown-prompt
                               yas/ido-prompt
                               yas/x-prompt
                               yas/completing-prompt
                               yas/no-prompt))
  (yas/load-directory (concat user-elisp-directory "yasnippet/snippets/")))

;; Default compile command-I don't like make -k
(setq-default compile-command "make ")


;;; CUSTOM COMMANDS

(global-set-key "\C-cu" 'calc)
(global-set-key "\C-cm" 'timestamp-insert)
(global-set-key "\C-cg" 'create-tags)
(global-set-key "\C-cp" 'compile)
(global-set-key "\C-cf" 'auto-fill-mode)


;;; EXTERNAL PROGRAMS

(defvar ctags-program-name "ctags")


;;; EDIFF

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;; PAREDIT

(add-hook 'paredit-mode-hook
          (lambda ()
            (local-set-key (kbd "M-[") 'paredit-wrap-square)))


;;; CEDET

(when (featurep 'semantic)
  (global-semantic-idle-completions-mode 1)
  (global-semantic-decoration-mode 0)
  (global-semantic-highlight-func-mode 0)
  (global-semantic-show-unmatched-syntax-mode 0)

  (global-ede-mode 1)

  (define-key semantic-mode-map (kbd "C-c , .") 'semantic-ia-fast-jump))


;;; ECB

(when (featurep 'ecb)
  (setq ecb-tip-of-the-day nil))


;;; TRAMP

;; Assume SCP if no explicit method
(setq tramp-default-method "scp")


;;; GDB

;; For the consistency of gdb-select-window's calling convention...
(defun gdb-comint-buffer-name ()
  (buffer-name gud-comint-buffer))
(defun gdb-source-buffer-name ()
  (buffer-name (window-buffer gdb-source-window)))

(defun gdb-select-window (header)
  "Switch directly to the specified GDB window.
Moves the cursor to the requested window, switching between
`gdb-many-windows' \"tabs\" if necessary in order to get there.

Recognized window header names are: 'comint, 'locals, 'registers,
'stack, 'breakpoints, 'threads, and 'source."

  (interactive "Sheader: ")

  (let* ((header-alternate (case header
                             ('locals      'registers)
                             ('registers   'locals)
                             ('breakpoints 'threads)
                             ('threads     'breakpoints)))
         (buffer (intern (concat "gdb-" (symbol-name header) "-buffer")))
         (buffer-names (mapcar (lambda (header)
                                 (funcall (intern (concat "gdb-"
                                                          (symbol-name header)
                                                          "-buffer-name"))))
                               (if (null header-alternate)
                                   (list header)
                                 (list header header-alternate))))
         (window (if (eql header 'source)
                     gdb-source-window
                   (or (get-buffer-window (car buffer-names))
                       (when (not (null (cadr buffer-names)))
                         (get-buffer-window (cadr buffer-names)))))))

    (when (not (null window))
      (let ((was-dedicated (window-dedicated-p window)))
        (select-window window)
        (set-window-dedicated-p window nil)
        (when (member header '(locals registers breakpoints threads))
          (switch-to-buffer (gdb-get-buffer-create buffer))
          (setq header-line-format (gdb-set-header buffer)))
        (set-window-dedicated-p window was-dedicated))
      t)))

;; Use global keybindings for the window selection functions so that they
;; work from the source window too...
(mapcar (lambda (setting)
          (lexical-let ((key    (car setting))
                        (header (cdr setting)))
            (global-set-key (concat "\C-c\C-g" key) #'(lambda ()
                                                        (interactive)
                                                        (gdb-select-window header)))))
        '(("c" . comint)
          ("l" . locals)
          ("r" . registers)
          ("u" . source)
          ("s" . stack)
          ("b" . breakpoints)
          ("t" . threads)))


;;; ORG MODE / DIARY

(load-file (concat user-emacs-directory "init-org.el"))


;;; ERC

(require 'erc)
(add-hook 'erc-mode-hook
          (lambda ()
            (make-local-variable 'scroll-margin)
            (setq scroll-margin 0)))

;; Keep the prompt at the bottom of the window
(erc-scrolltobottom-mode 1)

;; Don't show join/part messages.
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; Better completion.
(setq erc-pcomplete-nick-postfix ": ")

;; Only add ERC channels to the modeline when your nick is mentioned (taken
;; from http://www.emacswiki.org/emacs/ErcChannelTracking)
(setq erc-format-query-as-channel-p t
      erc-track-priority-faces-only 'all
      ;erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE" "333" "353")
      erc-track-faces-priority-list '(erc-error-face
                                      erc-current-nick-face
                                      erc-keyword-face
                                      erc-nick-msg-face
                                      erc-direct-msg-face
                                      erc-dangerous-host-face
                                      erc-notice-face
                                      erc-prompt-face)
      erc-fill-column 100)

;; ;; Enable chat logging
;; (add-to-list 'erc-modules 'log)
;; (setq erc-log-channels-directory "~/erclog/"
;;       erc-save-buffer-on-part t
;;       erc-log-insert-log-on-open nil)

;; Automatically save ERC buffers when exiting Emacs
;; (defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
;;   (save-some-buffers t (lambda ()
;;                          (when (eq major-mode 'erc-mode) t))))


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

;; Diary mode...
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
              (make-local-variable 'tab-stop-list)
              (setq tab-stop-list (simple-tab-stop-list 2 75)))))

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

;; NASM mode...
(add-to-list 'auto-mode-alist '("\\.\\(asm\\|s\\)$" . nasm-mode))
(add-hook 'nasm-mode-hook
          (lambda ()
            (make-local-variable 'tab-stop-list)
            (make-local-variable 'tab-always-indent)
            (setq tab-stop-list 8
                  tab-always-indent nil
                  indent-tabs-mode t)))

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
            (setq show-trailing-whitespace t)))
(add-hook 'inferior-python-mode-hook
          (lambda ()
            (make-local-variable 'scroll-margin)
            (setq scroll-margin         0
                  comint-process-echoes t)))
(when (featurep 'pymacs)
  (pymacs-load "ropemacs" "rope-")
  (setq ropemacs-enable-autoimport t))
(setq python-check-command "pyflakes")

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
(define-key text-mode-map "\C-cn" 'new-journal-entry)
(define-key text-mode-map "\C-c\C-o" 'org-open-at-point)
(define-key text-mode-map "\C-ct" 'artist-mode)
(define-key text-mode-map (kbd "TAB") 'tab-to-tab-stop)
(add-hook 'text-mode-hook
          (lambda ()
            (make-local-variable 'tab-stop-list)
            (setq tab-width 8
                  tab-stop-list '(4 8 16 24 32 40 48 56 64 72 80 88 96 104 112 120)
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
            (flyspell-enable)
            (search-forward "-- ")
            (previous-line)
            (open-line 1)))

;; Gnus

;; (require 'epa-file)
;; (epa-file-enable)
;; (setq epg-debug t)

(setq mm-decrypt-option 'always
      mm-verify-option 'always
      mml-smime-use 'epg
      gnus-buttonized-mime-types '("multipart/encrypted" "multipart/signed")

      gnus-save-newsrc-file nil
      gnus-read-newsrc-file nil)

(setq gnus-parameters
      '((".*"
         (gcc-self . none))))

;; Inspired by https://www.emacswiki.org/emacs/GnusFormatting
(setq-default gnus-group-line-format "%M%S%p%P%8y:%B%(%g%)\n"
              gnus-summary-line-format "%U%R%z %(%&user-date;  %-25,25f  %B%s%)\n"
              gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
              gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
              gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
              gnus-sum-thread-tree-false-root ""
              gnus-sum-thread-tree-indent " "
              gnus-sum-thread-tree-leaf-with-other "├► "
              gnus-sum-thread-tree-root ""
              gnus-sum-thread-tree-single-leaf "╰► "
              gnus-sum-thread-tree-vertical "│")

;; Shell and Term mode...
(add-hook 'shell-mode-hook #'zero-scroll-margin)
(add-hook 'term-mode-hook #'zero-scroll-margin)


;;; CUSTOM EXTENDED COMMANDS

(defun kill-file-name ()
  "Copies the full path of the current buffer."

  (interactive)
  (kill-new (buffer-file-name)))

(defun dump-variables ()
  "Dumps values of all symbols bound within the current scope."

  (let ((variables (loop for x being the symbols
                         if (boundp x)
                         collect (cons (symbol-name x)
                                       (eval (car (read-from-string (symbol-name x))))))))
    variables))

(defun print-variables ()
  "Prints values of all symbols bound within the current scope."

  (interactive)
  (loop for var in (dump-variables)
        do (insert (format "%s = %s" (car var) (cdr var)))))

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


;; Yoinked from http://stackoverflow.com/questions/1242352/get-font-face-under-cursor-in-emacs
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))


;; Generate tags for directory with Exuberant Ctags
(defun create-tags (dir-name)
  "Create tags file with Exuberant Ctags."
  (interactive "DDirectory: ")

  ;; We have to manually expand the tilde for Windows...
  (let ((dir-file (replace-regexp-in-string "^~"
                                            (replace-regexp-in-string "\\\\"
                                                                      "/"
                                                                      (getenv "HOME"))
                                            (directory-file-name dir-name))))
    (shell-command (format "\"%s\" -f \"%s/TAGS\" -e -R \"%s\""
                   ctags-program-name
                   dir-file
                   dir-file))))


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


;; Count words in region (like M-= except for words)
(defun wc ()
  (interactive)
  (message "Word count: %s" (how-many "\\w+" (point-min) (point-max))))

;; Support flyspell correction menus on the command line with popup.el if
;; it is available.
(when (featurep 'popup)
  ;; From http://www.emacswiki.org/FlySpell
  (defun flyspell-emacs-popup-textual (event poss word)
    "A textual flyspell popup menu."
    (let* ((corrects (if flyspell-sort-corrections
                         (sort (car (cdr (cdr poss))) 'string<)
                       (car (cdr (cdr poss)))))
           (cor-menu (if (consp corrects)
                         (mapcar (lambda (correct)
                                   (list correct correct))
                                 corrects)
                       '()))
           (affix (car (cdr (cdr (cdr poss)))))
           show-affix-info
           (base-menu  (let ((save (if (and (consp affix) show-affix-info)
                                       (list
                                        (list (concat "Save affix: " (car affix))
                                              'save)
                                        '("Accept (session)" session)
                                        '("Accept (buffer)" buffer))
                                     '(("Save word" save)
                                       ("Accept (session)" session)
                                       ("Accept (buffer)" buffer)))))
                         (if (consp cor-menu)
                             (append cor-menu (cons "" save))
                           save)))
           (menu (mapcar
                  (lambda (arg) (if (consp arg) (car arg) arg))
                  base-menu)))
      (cadr (assoc (popup-menu* menu :scroll-bar t) base-menu))))

  ;; (eval-after-load "flyspell"
  ;;   '(progn
  ;;      (fset 'flyspell-emacs-popup 'flyspell-emacs-popup-textual)))

  (eval-after-load "flyspell"
    '(progn
       (defadvice flyspell-emacs-popup (around flyspell-emacs-popup-choose
                                               (event poss word))
         "Use popup.el for flyspell correction if windowing system is absent."
         (if (window-system)
             ad-do-it
           (flyspell-emacs-popup-textual event poss word)))
       (ad-activate 'flyspell-emacs-popup))))

;; Consolidate flyspell commands
(defun flyspell-enable ()
  "Enable flyspell for the current buffer"

  (interactive)

  (flyspell-mode 1)
  (flyspell-buffer))

;; Taken from: http://www.emacswiki.org/emacs/Scrolling
;; (Emacs 23 doesn't have scroll-down-line?)
(defun mshroyer/scroll-down ()
  "Scroll down one line"
  (interactive)
  (scroll-up 1))

(defun mshroyer/scroll-up ()
  "Scroll up one line"
  (interactive)
  (scroll-down 1))

;; Borrowed from: http://goo.gl/Q3qpr
(defun mrc-xwin-look (frame)
  "Setup to use if running in an X window"
  (when (and (featurep 'color-theme)
             (boundp 'color-theme-local)
             (not (null color-theme-local))
             (window-system))
    (color-theme-initialize)
    (funcall color-theme-local)))

(defun mrc-terminal-look (frame)
  "Setup to use if running in a terminal")

(defun mrc-setup-frame (frame)
  (set-variable 'color-theme-is-global nil)
  (select-frame frame)
  (cond
   ((window-system)
    (mrc-xwin-look frame)
    (tool-bar-mode -1))
   (t (mrc-terminal-look frame))))

(add-hook 'after-make-frame-functions 'mrc-setup-frame)

(add-hook 'after-init-hook
          (lambda ()
            (mrc-setup-frame (selected-frame))))


;;; EDITOR SERVERS

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
  (add-hook 'server-done-hook
            (if (>= emacs-major-version 23)

                ;; It seems that Emacs 23 takes care of closing the
                ;; emacslcient frame for us, so if we're using that version
                ;; we don't need to explicitly delete the frame; doing so
                ;; will inadvertently delete the last used GUI emacsclient
                ;; frame as well.
                (lambda ()
                  (kill-buffer nil)
                  (redraw-display))

              (lambda ()
                (kill-buffer nil)
                (delete-frame)
                (redraw-display)))))

;; Chrome "Edit with Emacs" server
;; https://chrome.google.com/extensions/detail/ljobjlafonikaiipfkggjbhkghgicgoh
(when (and (featurep 'edit-server)
           (should-start-server :chrome-edit))

  (edit-server-start)

  ;; Enable word wrap in the edit window
  (add-hook 'edit-server-text-mode-hook
            (lambda ()
              (auto-fill-mode nil)
              (longlines-mode t))))


;;; RESTORE DESKTOP

;(desktop-save-mode 1)


;; Custom variables from the Emacs configuration editor
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-verbatim-environments (quote ("verbatim" "verbatim*" "Verbatim")))
 '(ecb-layout-window-sizes nil)
 '(ecb-options-version "2.40")
 '(gnus-thread-sort-functions (quote (gnus-thread-sort-by-most-recent-date)) t)
 '(gnus-treat-display-smileys nil)
 '(helm-split-window-in-side-p t)
 '(package-selected-packages (quote (eclim)))
 '(safe-local-variable-values (quote ((TeX-master . "manual") (TeX-master . t))))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:inherit nil))))
 '(isearch ((t (:background "pale violet red" :foreground "white"))))
 '(trailing-whitespace ((t (:background "khaki3" :foreground "black")))))
