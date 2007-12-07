;;;
;;; ~/.emacs
;;;
;;; My global Emacs configuration file.  Contains some settings
;;; specific to Mac OS X.
;;;
;;; Mark Shroyer
;;; http://markshroyer.com/
;;;

;;; SYSTEM

;; Use Aspell for spell checking
;(setq ispell-program-name "aspell")

;; Start server mode if we're running in a windowing environment
(if window-system (server-start))

;; Configure elisp load path to include my ~/.emacs.d/ files
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/elisp/")
	   (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))


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

;; Color themes!  (But only when running in a GUI, of course...)
(if window-system
    (progn
      (require 'color-theme)
      (color-theme-initialize)
      (color-theme-classic)))


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


;;; CUSTOM MODE HOOKS

;; Text mode...
(add-hook 'text-mode-hook
          (lambda ()
            (paragraph-indent-minor-mode)
;            (flyspell-mode)             ; Perform spell checking in
;                                        ; text-mode buffers by default
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
