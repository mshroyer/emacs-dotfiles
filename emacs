;;;
;;; ~/.emacs
;;;
;;; My global Emacs configuration file.  Contains some settings
;;; specific to Mac OS X.
;;;
;;; Mark Shroyer
;;; http://markshroyer.com/
;;;

;;; UTILITY

;; Generate a simple list of tab stops, 'stop-width characters in between
;; and up to 'text-width characters long in total.
(defun simple-tab-stop-list (stop-width text-width)
  "Generate a simple list of tab stops"

  (do ((stop stop-width (+ stop stop-width))
        (stop-list nil))
      ((> stop text-width) stop-list)
    (setq stop-list (append stop-list (list stop)))))


;;; SYSTEM

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

;; Configure elisp load path to include my ~/.emacs.d/ files
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/elisp/")
	   (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))


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

;; Use text mode as the default for new buffers
(setq default-major-mode 'text-mode)

;; Delete unnecessary auto-save files
(setq delete-auto-save-files t)

;; Don't leave those damn annoying backup files all over the place (I don't
;; know how to emulate Vim's backupcopy=yes mode)
(setq make-backup-files nil)

;; Make sure the last line of a file ends in a carriage return
(setq require-final-newline t)

;;; GENERAL INTERFACE SETTINGS

;; Don't display startup message
(setq inhibit-startup-message t)

;; Get rid of the toolbar
(tool-bar-mode 0)

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
;(fset 'yes-or-no-p 'y-or-n-p)

;; Scroll one line at a time, like Vim
(setq scroll-step 1)

;; Color themes!  (But only when running in a GUI, of course...)
(if window-system
    (progn
      (require 'color-theme)
      (color-theme-initialize)
      (if (fboundp 'color-theme-local)  ; Use the local color theme if one
          (color-theme-local)           ; was defined in ~/.emacs.local
        (color-theme-classic))))


;;; SCMs

;(require 'git-emacs)


;;; EDITING OPTIONS

;; Viper mode!
;(setq viper-mode t)
;(require 'viper)

;; Text mode abbreviations
(setq-default abbrev-mode t)
(read-abbrev-file "~/.abbrev_defs")
(setq save-abbrevs t)

;; Set up syntax coloring
(global-font-lock-mode t)

;; Turn on paren matching (this is a Lisp editor, is it not?)
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; Use spaces for indentation, not tab chracters
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
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

;; Recheck spelling after running ispell-key keyboard macro
;(global-set-key "\M-$" 'flyspell-word)

;; Allow easy saving and restoration of point to and from registers
(global-set-key "\C-x r p" 'register-to-point)


;;; EXTENSIONS

(require 'slime)
(slime-setup)


;;; CUSTOM MODE HOOKS AND SETTINGS

;; Outline mode...
(add-to-list 'auto-mode-alist '("\\.ol$" . outline-mode))

;; HTML mode...
(add-to-list 'auto-mode-alist '("\\.mtml$" . html-mode))
(add-hook 'html-mode-hook
          (lambda ()
            (auto-fill-mode nil)
            (setq tab-width 2)))

;; Markdown mode...
(autoload 'markdown-mode "markdown-mode.el")
(setq auto-mode-alist
      (append '(("\\.mkd$" . markdown-mode)
                ("\\.markdown$" . markdown-mode))
              auto-mode-alist))
(add-hook 'markdown-mode-hook
          (lambda ()
            (auto-fill-mode)))

;; YAML mode...
(add-hook 'yaml-mode-hook
          (lambda ()
            (local-set-key "\C-cn" 'new-yaml-ab-entry)
            (setq indent-tabs-mode nil)
            (setq tab-stop-list (simple-tab-stop-list 2 75))))

;; C mode...
(add-hook 'c-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)))

;; ASM mode...
(add-hook 'asm-mode-hook
          (lambda ()
            (setq tab-width 3)
            (setq tab-stop-list (simple-tab-stop-list 3 75))
            (setq indent-tabs-mode t)
            (local-set-key (kbd "TAB") 'tab-to-tab-stop)))

;; Perl mode...
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
(add-hook 'cperl-mode-hook
          (lambda ()
            (setq cperl-indent-level 4)
            (setq cperl-continued-statement-offset 8)))

;; Python mode...
(add-hook 'python-mode-hook
          (lambda ()
            (abbrev-mode nil)))

;; JavaScript mode...
(autoload 'javascript-mode "javascript" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))

;; Common Lisp mode...
(add-hook 'lisp-mode-hook
          (lambda ()
            (setq lisp-indent-function 'common-lisp-indent-function)))

;; Text mode...
(define-key text-mode-map (kbd "TAB") 'self-insert-command)
(define-key text-mode-map "\C-m" 'newline)
(define-key text-mode-map "\C-j" 'newline-and-indent)
(define-key text-mode-map "\C-cn" 'new-journal-entry)
(add-hook 'text-mode-hook
          (lambda ()
            (paragraph-indent-minor-mode)
            (setq tab-width 8)
            (auto-fill-mode t)))


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
  (insert (format-time-string "%a %b %e %H:%M:%S %Z %Y"))
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

  (flyspell-mode t)
  (flyspell-buffer))

(defun flyspell-word ()
  "Check the current word's spelling and then re-run flyspell"

  (interactive)

  (command-execute 'ispell-word)
  (flyspell-buffer))
