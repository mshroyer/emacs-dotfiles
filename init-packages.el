;;; -*- lexical-binding: t -*-
;;;
;;; Package configuration and dependencies.

;; Package archives
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)

;; Package configurations.
(when (require 'helm nil t)
  (global-set-key "\C-ch" 'helm-command-prefix)
  (helm-mode 1)
  (global-set-key "\C-xb" 'helm-mini)
  (global-set-key "\C-x\C-f" 'helm-find-files)

  ;; Automatically scale helm window to fit its contents.
  (helm-autoresize-mode t)

  ;; Increase the width of the buffer name column.
  (setq helm-buffer-max-length 32)

  ;; Don't use helm for M-x.
  (add-to-list 'helm-completing-read-handlers-alist '(execute-extended-command . nil))
  (add-to-list 'helm-completing-read-handlers-alist '(ff-find-other-file . nil))
  (add-to-list 'helm-completing-read-handlers-alist '(woman . nil))

  ;; Additional bindings.
  (global-set-key "\M-y" 'helm-show-kill-ring)
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t)

  ;; Show helm at the bottom of the frame
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*helm" (* not-newline) "*" eos)
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (window-height . 0.4))))

(when (require 'magit nil t)
  (global-set-key "\C-ct" 'magit-status))

(when (require 'undo-tree nil t)
  (global-undo-tree-mode 1))

(when (require 'expand-region nil t)
  (global-set-key "\C-c=" 'er/expand-region))

(when (require 'nasm-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.\\(asm\\|s\\)$" . nasm-mode))
  (add-hook 'nasm-mode-hook
            (lambda ()
              (make-local-variable 'tab-stop-list)
              (make-local-variable 'tab-always-indent)
              (setq tab-stop-list 8
                    tab-always-indent nil
                    indent-tabs-mode t))))

(require 'lua-mode nil t)
(require 'web-mode nil t)
