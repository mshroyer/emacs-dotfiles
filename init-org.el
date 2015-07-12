(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-ck" 'mshroyer/org-show-unblocked-todo-tree)
(global-set-key "\C-ci" 'mshroyer/org-show-inbox)

;; Terminal compatibility

; Backtab in terminal emulators such as gnome-terminal, konsole, etc.:
(define-key org-mode-map "\M-[z" 'org-shifttab)

; Because we can't use C-return and C-S-return in the terminal:
(define-key org-mode-map (kbd "C-M-j") 'mshroyer/org-insert-heading)

(defun mshroyer/org-insert-heading (&optional arg)
  "Invoke desired heading function depending on parameter"
  (interactive "P")
  (if arg
      (org-insert-todo-heading-respect-content)
    (org-meta-return)))  ; Calls org-insert-heading or org-table-wrap-region

; Use C-ct as an alternative for C-cC-t, so that we don't have to use quite
; as many keystrokes with GNU Screen's escape bound to C-t
(define-key org-mode-map "\C-ct" 'org-todo)
(setq org-cycle-separator-lines 2
      org-special-ctrl-a/e t
      org-agenda-start-on-weekday 0
      org-completion-use-ido t
      org-agenda-window-setup 'current-window)

; Don't re-evaluate code blocks when exporting.
(setq org-export-babel-evaluate nil)

; Always show context when creating sparse trees:
(setq org-show-siblings t)

(add-hook 'org-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (auto-fill-mode 1)))

(setq org-agenda-files (mapcar (lambda (file)
                                 (concat org-directory file))
                               '("/todo.org"))
      diary-file (concat org-directory "/diary")

      org-agenda-include-diary t
      org-enforce-todo-dependencies t
      org-agenda-dim-blocked-tasks nil
      org-agenda-todo-ignore-scheduled nil
      org-agenda-remove-tags 'prefix
      org-deadline-warning-days 7
      org-stuck-projects '("+LEVEL=1/-DONE"
                           ("TODO" "WAIT")
                           nil
                           nil)

      org-agenda-custom-commands
      '(("a" agenda "Agenda")
        ("w" "Waiting-for items by context" todo "WAIT"
         ((org-agenda-sorting-strategy '(todo-state-up tag-up time-up))
          (org-agenda-prefix-format "")
          (org-agenda-todo-keyword-format "")
          (org-agenda-skip-function 'mshroyer/org-skip-inactive)))
        ("d" "Non-dated action items by context" todo "TODO"
         ((org-agenda-sorting-strategy '(todo-state-up tag-up time-up))
          (org-agenda-prefix-format "%16T: %(mshroyer/org-agenda-todo-prefix)")
          (org-agenda-todo-keyword-format "")
          (org-agenda-skip-function 'mshroyer/org-skip-inactive)))
        ("p" "Project action items" todo-tree "TODO")))

(defun mshroyer/org-project-for-path (path)
  "Get a project name for the given org path

Returns a project name corresponding to the given org path (as
defined by my personal todo.org layout), or nil if the item at
point is not part of a project."
  (let ((heading (car path)))
    (if (string= heading "Misc") nil
      heading)))

(defun mshroyer/org-agenda-todo-prefix ()
  "Generate Org agenda prefix with extra context

Builds TODO item prefixes including the project name, if any, and
any non-final tags."
  (let ((project       (mshroyer/org-project-for-path (org-get-outline-path)))
        (extra-context (mapcar (lambda (tagname)
                                 (concat tagname ":"))
                               (cdr (reverse (org-get-tags))))))
    (mapconcat (lambda (str)
                 (concat " " str))
               (append extra-context (if project
                                         (list (concat "[" project "]"))))
               "")))

(defun mshroyer/org-todo-active-p ()
  "Determines whether the current todo item is active

Returns non-nil if the todo item currently under the point can
currently be worked on; returns nil if the item is blocked from
completion by either dependency on another todo item or because
it is scheduled at a future timestamp.

We use this as a custom skip function for org todo views rather
than just setting org-agenda-dim-blocked-tasks because we still
want to show blocked deadline tasks on the agenda view.
Likewise, we use a custom implementation of future scheduled
tasks logic rather than set org-agenda-todo-ignore-scheduled to
'future because, because that only ignores tasks on future dates;
it doesn't work for future timestamps on the current date."
  (let* ((subtree-end (save-excursion
                        (org-end-of-subtree t)))
         (scheduled-time (save-excursion
                           (if (re-search-forward org-scheduled-time-regexp
                                                  subtree-end t)
                               (org-time-string-to-time (match-string 1))
                             nil)))
         (time> (lambda (a b)
                  (cond ((not (car a))       nil)
                        ((not (car b))       t)
                        ((< (car a) (car b)) nil)
                        ((> (car a) (car b)) t)
                        (t (funcall time> (cdr a) (cdr b)))))))
    (save-excursion
      (and (reduce (lambda (a b)
                     (and a b))
                   (mapcar (lambda (f)
                             (funcall (symbol-function f)
                                      '(:type todo-state-change :to done)))
                           org-blocker-hook))
           (not (and scheduled-time
                     (funcall time> scheduled-time (org-current-time))))))))

(defun mshroyer/org-skip-inactive ()
  "Skip function based on mshroyer/org-todo-active-p"
  (let ((subtree-end (save-excursion
                       (org-end-of-subtree t))))
    (if (mshroyer/org-todo-active-p)
        nil
      subtree-end)))

(defun mshroyer/org-show-inbox ()
  "Show the Org Mode GTD inbox file"
  (interactive)
  (find-file (concat org-directory "/inbox.org")))

(defun mshroyer/org-show-unblocked-todo-tree ()
  "Show currently unblocked action items

Builds a sparse tree which highlights only action items which are
not blocked by other tasks and which are not scheduled into the
future."
  (interactive)
  (find-file (concat org-directory "/todo.org"))
  (org-occur (concat "^" org-outline-regexp " *" org-not-done-regexp)
             nil
             'mshroyer/org-todo-active-p))

;; Setup org capture, but only if we're using a newer version of Org Mode
;; that includes this feature...
(when (require 'org-capture nil t)
  (setq org-default-notes-file (concat org-directory "/inbox.org")
        org-capture-templates
        '(("i" "Inbox" entry (file (concat org-directory "/inbox.org"))
           "* %?\n"))
        org-refile-use-outline-path t
        org-outline-path-complete-in-steps nil
        org-refile-targets
        `(((,(concat org-directory "/todo.org")) . (:level . 1))))
  (define-key global-map "\C-cc" 'org-capture))

; Only show holidays that I actually care about
(setq calendar-holidays
      '((holiday-fixed 1 1 "New Year's Day")
        (holiday-float 1 1 3 "Martin Luther King Day")
        (holiday-fixed 2 2 "Groundhog Day")
        (holiday-fixed 2 14 "Valentine's Day")
        (holiday-float 2 1 3 "President's Day")
        (holiday-fixed 3 17 "St. Patrick's Day")
        (holiday-fixed 4 1 "April Fools' Day")
        (holiday-float 5 0 2 "Mother's Day")
        (holiday-float 5 1 -1 "Memorial Day")
        (holiday-fixed 6 14 "Flag Day")
        (holiday-float 6 0 3 "Father's Day")
        (holiday-fixed 7 4 "Independence Day")
        (holiday-float 9 1 1 "Labor Day")
        (holiday-float 10 1 2 "Columbus Day")
        (holiday-fixed 10 31 "Halloween")
        (holiday-fixed 11 11 "Veteran's Day")
        (holiday-float 11 4 4 "Thanksgiving")
        (holiday-easter-etc)
        (holiday-fixed 12 25 "Christmas")
        (holiday-chinese-new-year)
        (solar-equinoxes-solstices)
        (holiday-sexp calendar-daylight-savings-starts
                      (format "Daylight Saving Time Begins %s"
                              (solar-time-string
                               (/ calendar-daylight-savings-starts-time
                                  (float 60))
                               calendar-standard-time-zone-name)))
        (holiday-sexp calendar-daylight-savings-ends
                      (format "Daylight Saving Time Ends %s"
                              (solar-time-string
                               (/ calendar-daylight-savings-ends-time
                                  (float 60))
                               calendar-daylight-time-zone-name)))))
