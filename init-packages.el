;;; -*- lexical-binding: t -*-
;;;
;;; Package configuration and dependencies.

;; Package archives
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)

(defmacro ensure-package (name &rest args)
  "Use the package and ensure it is installed.

This macro wraps `use-package` to ensure the given package is
installed, and also that it is included in
`package-selected-packages` so that it will not be removed by
`package-autoremove`.

In theory, use-package should make sure ensured packages get
added to package-selected-packages automatically. However, this
doesn't work out with Emacs 26.1 and use-package 2.4. This may be
https://github.com/jwiegley/use-package/issues/327, but I didn't
dig too deeply.

So I'll just keep using this macro for now. It's cool."
  (macroexp-progn
   `((use-package ,name :ensure t ,@args)
     (add-to-list 'package-selected-packages ',name))))

(when (require 'use-package nil t)
  (ensure-package diminish)

  (ensure-package helm
    :diminish helm-mode
    :bind (("C-c h" . helm-command-prefix)
           ("C-x b" . helm-mini)
           ("C-x C-f" . helm-find-files)
           ("M-y" . helm-show-kill-ring))
    :init
    (setf helm-buffer-max-length 32
          helm-buffers-fuzzy-matching t
          helm-recentf-fuzzy-match t)
    :config
    (helm-mode 1)
    (helm-autoresize-mode t)

    ;; Don't use helm for M-x.
    (add-to-list 'helm-completing-read-handlers-alist '(execute-extended-command . nil))
    (add-to-list 'helm-completing-read-handlers-alist '(ff-find-other-file . nil))
    (add-to-list 'helm-completing-read-handlers-alist '(woman . nil))

    ;; Show helm at the bottom of the frame
    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*helm" (* not-newline) "*" eos)
                   (display-buffer-in-side-window)
                   (inhibit-same-window . t)
                 (window-height . 0.4))))

  (ensure-package helm-org-rifle
                  :bind ("C-c f" . helm-org-rifle))

  (ensure-package magit
    :bind ("C-c t" . magit-status))

  (ensure-package undo-tree
    :diminish undo-tree-mode
    :config
    (global-undo-tree-mode 1))

  (ensure-package expand-region
    :bind ("C-c =" . er/expand-region))

  (ensure-package nasm-mode
    :init
    (add-to-list 'auto-mode-alist '("\\.\\(asm\\|s\\)$" . nasm-mode))
    (add-hook 'nasm-mode-hook (lambda ()
				(make-local-variable 'tab-stop-list)
				(make-local-variable 'tab-always-indent)
				(setq tab-stop-list 8
				      tab-always-indent nil
				      indent-tabs-mode t))))

  (ensure-package lua-mode)

  (ensure-package web-mode)

  (ensure-package markdown-mode)

  (ensure-package elpy
    :config
    (elpy-enable))

  (ensure-package go-mode)

  (ensure-package auctex))
