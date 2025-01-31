;;; -*- lexical-binding: t -*-
;;;
;;; Package configuration and dependencies.

;; Package archives
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

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

  (ensure-package color-theme-modern)

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
                  (add-to-list 'helm-completing-read-handlers-alist
                               '(execute-extended-command . nil))
                  (add-to-list 'helm-completing-read-handlers-alist
                               '(ff-find-other-file . nil))
                  (add-to-list 'helm-completing-read-handlers-alist
                               '(woman . nil))

                  ;; Show helm at the bottom of the frame
                  (add-to-list 'display-buffer-alist
                               `(,(rx bos "*helm" (* not-newline) "*" eos)
                                 (display-buffer-in-side-window)
                                 (inhibit-same-window . t)
                                 (window-height . 0.4))))

  (ensure-package helm-org-rifle)

  (ensure-package org-roam
                  :bind (("C-c n l" . org-roam-buffer-toggle)
                         ("C-c n f" . org-roam-node-find)
                         ("C-c n i" . org-roam-node-insert))

                  :init
                  ;; Suppress notification about migrating to v2.
                  (setq org-roam-v2-ack t)

                  :config
                  (org-roam-setup)
                  (org-roam-db-autosync-mode))

  (ensure-package projectile
                  :diminish projectile-mode
                  :init
                  (setf projectile-keymap-prefix (kbd "C-c p")
                        projectile-completion-system 'helm)
                  :config
                  (projectile-global-mode))

  (ensure-package rg)
  (ensure-package helm-rg)

  (ensure-package magit
                  :bind ("C-c t" . magit-status))

  (ensure-package undo-tree
                  :diminish undo-tree-mode
                  :config
                  (global-undo-tree-mode 1)
                  (setq undo-tree-auto-save-history nil))

  (ensure-package expand-region
                  :bind ("C-c =" . er/expand-region))

  (ensure-package nasm-mode
                  :init
                  (add-to-list 'auto-mode-alist
                               '("\\.\\(asm\\|s\\)$" . nasm-mode))
                  (add-hook 'nasm-mode-hook
                            (lambda ()
                              (make-local-variable 'tab-stop-list)
                              (make-local-variable 'tab-always-indent)
                              (setq tab-width 24
                                    tab-always-indent nil
                                    indent-tabs-mode t))))

  (ensure-package lsp-mode
                  :commands lsp
                  :custom
                  ;; what to use when checking on-save. "check" is default, I prefer clippy
                  (lsp-rust-analyzer-cargo-watch-command "clippy")
                  (lsp-eldoc-render-all nil)
                  (lsp-eldoc-enable-hover nil)
                  (lsp-idle-delay 0.6)
                  ;; enable / disable the hints as you prefer:
                  (lsp-rust-analyzer-server-display-inlay-hints nil)
                  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
                  (lsp-rust-analyzer-display-chaining-hints t)
                  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
                  (lsp-rust-analyzer-display-closure-return-type-hints t)
                  (lsp-rust-analyzer-display-parameter-hints nil)
                  (lsp-rust-analyzer-display-reborrow-hints nil)
                  :config
                  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

  (ensure-package lsp-ui
                  :commands lsp-ui-mode
                  :custom
                  (lsp-ui-peek-always-show t)
                  (lsp-ui-sideline-show-hover t)
                  (lsp-ui-doc-enable nil))

  (ensure-package lua-mode)

  (ensure-package web-mode)

  (ensure-package markdown-mode)

  (ensure-package elpy
                  :config
                  (elpy-enable))

  (ensure-package go-mode
                  :config
                  (add-hook 'go-mode-hook
                            (lambda ()
                              (add-hook 'before-save-hook #'gofmt-before-save nil))))

  (ensure-package rust-mode)

  (ensure-package rustic
                  :bind (:map rustic-mode-map
                              ("M-j" . lsp-ui-imenu)
                              ("M-?" . lsp-find-references)
                              ("C-c C-c l" . flycheck-list-errors)
                              ("C-c C-c a" . lsp-execute-code-action)
                              ("C-c C-c r" . lsp-rename)
                              ("C-c C-c q" . lsp-workspace-restart)
                              ("C-c C-c Q" . lsp-workspace-shutdown)
                              ("C-c C-c s" . lsp-rust-analyzer-status))
                  :config
                  ;; uncomment for less flashiness
                  ;(setq lsp-eldoc-hook nil)
                  ;;(setq eldoc-documentation-functions nil)
                  (setq lsp-enable-symbol-highlighting nil)
                  (setq lsp-signature-auto-activate nil)

                  ;; comment to disable rustfmt on save
                  (setq rustic-format-trigger 'on-save))

  (ensure-package company
    :custom
    (company-idle-delay 0.5) ;; how long to wait until popup
    ;; (company-begin-commands nil) ;; uncomment to disable popup
    :bind
    (:map company-active-map
	  ("C-n". company-select-next)
	  ("C-p". company-select-previous)
	  ("M-<". company-select-first)
	  ("M->". company-select-last)))

  (ensure-package yasnippet
    :config
    (yas-reload-all)
    (add-hook 'prog-mode-hook 'yas-minor-mode)
    (add-hook 'text-mode-hook 'yas-minor-mode))

  (ensure-package flycheck)

  (ensure-package haskell-mode)

  (ensure-package tuareg)

  (ensure-package cider)

  (ensure-package clojure-mode)

  (ensure-package erlang)

  (ensure-package slime)
  (ensure-package helm-slime)

  ;; Needed to prevent package signature check errors when accessing ELPA from
  ;; an older Emacs package.
  (ensure-package gnu-elpa-keyring-update)

  ;; AucTeX doesn't provide its package name.
  (use-package tex
    :ensure auctex)
  (add-to-list 'package-selected-packages 'auctex)

  ;; Don't need `ensure-package` because the mode is provided in ./elisp.
  (use-package dtrace-script-mode
    :config
    (add-to-list 'auto-mode-alist
                 '("\\.d$" . dtrace-script-mode))
    (add-hook 'dtrace-script-mode-hook
              (lambda ()
                (setf indent-tabs-mode t)))))
