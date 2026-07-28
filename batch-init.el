;;; Initialize Emacs from a batch command

;; Set user-init-file so that path processing in init.el works.
(setq user-init-file (concat (getenv "HOME") "/.emacs.d/init.el"))

(load-file user-init-file)
