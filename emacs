;;;
;;; ~/.emacs
;;;
;;; My global Emacs configuration file.  Contains some settings
;;; specific to Mac OS X.
;;;
;;; Mark Shroyer
;;; http://markshroyer.com/
;;;

;;; MAC OS X-SPECIFIC CONFIGURATIONS

;; Use the Option/Alt key for Meta in Emacs.app
(setq mac-command-modifier 'option)
(setq mac-option-modifier 'meta)


;;; FILE HANDLING

;; Use text mode as the default for new buffers
(setq default-major-mode 'text-mode)

;; Delete unnecessary auto-save files
(setq delete-auto-save-files t)

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

;; Don't make me type out long answers...
(fset 'yes-or-no-p 'y-or-n-p)

;; Scroll one line at a time, like Vim
(setq scroll-step 1)


;;; EDITING OPTIONS

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
(setq fill-column 75)

;; Show trailing whitespace
(if (>= emacs-major-version 21)
    (setq show-trailing-whitespace t))

;; Swap to C-j for raw newline, C-m for newline-and-indent
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-j" 'newline)
(global-set-key "\M-\C-m" 'indent-new-comment-line)


;;; CUSTOM MODE HOOKS

;; Text mode...
(add-hook 'text-mode-hook
          (lambda ()
            (paragraph-indent-minor-mode)
            (define-key text-mode-map (kbd "TAB") 'self-insert-command)
            (setq tab-width 8)
            (auto-fill-mode t)
            (define-key text-mode-map "\C-m" 'newline)
            (define-key text-mode-map "\C-j" 'newline-and-indent)))


;;; CUSTOM EXTENDED COMMANDS

;; Show an ASCII table
;; (Inspired by http://www-cdf.fnal.gov/~sthrlnd/emacs_help.html)
(defun ascii-table ()
  "Show a table of ASCII characters by decimal, hex, and octal value."

  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (let ((min 1) (max 255))
    (insert (format "ASCII characters %d-%d\n\n" min max))
    (insert " Dec   Hex   Oct    Character\n")
    (insert "========================================\n")
    (let ((i min))
      (while (<= i max)
        (let ((line "%4d  #x%02X  %04o    "))
          (insert (cond
                   ((= i 9) (format (concat line "(horizontal tab)\n") i i i))
                   ((= i 10) (format (concat line "(backspace)\n") i i i))
                   ((= i 12) (format (concat line "(form feed)\n") i i i))
                   ((= i 32) (format (concat line "(space)\n") i i i))
                   (t (format (concat line "%c\n") i i i i))))
          (setq i (+ i 1))))))
  (beginning-of-buffer))
