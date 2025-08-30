;;; -*- lexical-binding: t -*-
;;;
;;; mshroyer-lib.el
;;;
;;; My custom Emacs functions.

(require 'cl-lib)

(defun mshroyer--char (str i)
  "Return character at position i in str"
  (string-to-char (substring str i)))

(defun mshroyer--last-char (str)
  "Return last character in str"
  (mshroyer--char str (- (length str) 1)))

(defun mshroyer-flatten-path-tree (path-tree)
  "Flatten an assoc-list tree of paths.
Flattens an assoc-list tree of paths, such as the user-elisp
paths used in my emacs init, depth first into a plain list of
paths."
  (if (null path-tree)
      nil
    (let* ((sub-tree  (car path-tree))
           (this-dir  (if (eql (mshroyer--last-char (car sub-tree))
                               (string-to-char "/"))
                          (car sub-tree)
                        (concat (car sub-tree) "/"))))
      (cons this-dir
            (append (if (not (null (cdr sub-tree)))
                        (mapcar (lambda (sub-path)
                                  (concat this-dir sub-path))
                                (mshroyer-flatten-path-tree (cdr sub-tree))))
                    (mshroyer-flatten-path-tree (cdr path-tree)))))))

(defun mshroyer-add-frame-hook (hook)
  "Add a hook for all new frames, including during init.
Adds a hook to be run for all new frames, including the initial
frame being created during init."
  (add-hook 'after-make-frame-functions hook)
  (let ((frame (window-frame)))
    (add-hook 'after-init-hook (lambda ()
                                 (funcall hook frame)))))

;; (Inspired by http://www-cdf.fnal.gov/~sthrlnd/emacs_help.html)
(defun mshroyer-ascii-table ()
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
  (goto-char (point-min)))


(defun mshroyer-timestamp-string ()
  "Returns a Unix date(1)-format timestamp"
  (format-time-string "%a %b %e %H:%M:%S %Z %Y"))


(defun mshroyer-insert-timestamp ()
  "Inserts a Unix date(1)-format timestamp in the current buffer"

  (interactive)
  (insert (mshroyer-timestamp-string)))


;; Create a new journal entry
(defun mshroyer-new-journal-entry ()
  "Make a new journal entry with a Unix `date`-style timestamp"

  (interactive)
  (goto-char (point-max))
  (if (re-search-backward "[^ \t\n]" nil t)
      (progn
        (end-of-line)
        (let ((beg (point)))
          (goto-char (point-max))
          (delete-region beg (point)))
        (dotimes (_ 3)
          (newline))))
  (insert (mshroyer-timestamp-string))
  (dotimes (_ 2)
    (newline)))


;; Yoinked from http://stackoverflow.com/questions/1242352/get-font-face-under-cursor-in-emacs
(defun mshroyer-what-face (pos)
  "Describes the font face under the cursor"
  
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))


(defun mshroyer-kill-file-name ()
  "Copies the full path of the current buffer."

  (interactive)
  (kill-new (buffer-file-name)))


(defun mshroyer-dump-variables ()
  "Dumps values of all symbols bound within the current scope."

  (let ((variables (cl-loop for x being the symbols
                            if (boundp x)
                            collect (cons (symbol-name x)
                                          (eval (car (read-from-string (symbol-name x))))))))
    variables))


(defun mshroyer-print-variables ()
  "Prints values of all symbols bound within the current scope."

  (interactive)
  (cl-loop for var in (mshroyer-dump-variables)
           do (insert (format "%s = %s" (car var) (cdr var)))))


(defun mshroyer-flyspell-enable ()
  "Enable flyspell for the current buffer"

  (interactive)

  (flyspell-mode 1)
  (flyspell-buffer))


;; Generate tags for directory with Exuberant Ctags
(defun mshroyer-create-tags ()
  "Create tags file with etags."
  (interactive)
  (let ((etags-command (read-shell-command
                        "Run etags (like this): "
                        (format "cd '%s'; find . -type f -name '*.[ch]' | etags -" default-directory))))
    (eshell-command etags-command)))


;; http://dfan.org/blog/2009/02/19/emacs-dedicated-windows/
(defun mshroyer-toggle-current-window-dedication ()
  "Toggle the current window's dedication state"

  (interactive)
  (let* ((window    (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))


(defun mshroyer/toggle-comment ()
  "Toggles commenting of the current line or active region"

  (interactive)
  (let ((beginning (if (region-active-p)
                       (region-beginning)
                     (line-beginning-position)))
        (end (if (region-active-p)
                 (region-end)
               (line-end-position))))
    (if (comment-only-p beginning end)
        (uncomment-region beginning end)
      (comment-region beginning end))))


(provide 'mshroyer-lib)
