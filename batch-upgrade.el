;;; Upgrade all packages from a --batch command.

(load-file (concat (getenv "HOME") "/.emacs.d/batch-init.el"))
(package-upgrade-all)
