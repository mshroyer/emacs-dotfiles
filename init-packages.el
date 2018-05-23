;;; -*- lexical-binding: t -*-
;;;
;;; Package configuration and dependencies.

;; Package archives
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)

(when (featurep 'use-package)
  (use-package helm
    :ensure t
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

  (use-package magit
    :ensure t
    :bind ("C-c t" . magit-status))

  (use-package undo-tree
    :ensure t
    :config
    (global-undo-tree-mode 1))

  (use-package expand-region
    :ensure t
    :bind ("C-c =" . er/expand-region))

  (use-package nasm-mode
    :ensure t
    :init
    (add-to-list 'auto-mode-alist '("\\.\\(asm\\|s\\)$" . nasm-mode))
    (add-hook 'nasm-mode-hook (lambda ()
				(make-local-variable 'tab-stop-list)
				(make-local-variable 'tab-always-indent)
				(setq tab-stop-list 8
				      tab-always-indent nil
				      indent-tabs-mode t))))

  (use-package lua-mode
    :ensure t)

  (use-package web-mode
    :ensure t)

  (use-package elpy
    :ensure t))
