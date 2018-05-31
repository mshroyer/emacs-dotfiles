(require 'erc)

(add-hook 'erc-mode-hook
          (lambda ()
            (set (make-local-variable 'scroll-margin) 0)
            (set (make-local-variable 'scroll-conservatively) 100)))

;; Keep the prompt at the bottom of the window
(erc-scrolltobottom-mode 1)

;; Better completion.
(setf erc-pcomplete-nick-postfix ": ")

;; Only add ERC channels to the modeline when your nick is mentioned (taken
;; from http://www.emacswiki.org/emacs/ErcChannelTracking)
(setf erc-format-query-as-channel-p t
      erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE")
      erc-track-priority-faces-only 'all
      erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE" "333" "353")
      erc-track-faces-priority-list '(erc-error-face
                                      erc-current-nick-face
                                      erc-keyword-face
                                      erc-nick-msg-face
                                      erc-direct-msg-face
                                      erc-dangerous-host-face
                                      erc-notice-face
                                      erc-prompt-face)
      erc-fill-column 100)

;; ;; Enable chat logging
;; (add-to-list 'erc-modules 'log)
;; (setq erc-log-channels-directory "~/erclog/"
;;       erc-save-buffer-on-part t
;;       erc-log-insert-log-on-open nil)

;; Automatically save ERC buffers when exiting Emacs
;; (defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
;;   (save-some-buffers t (lambda ()
;;                          (when (eq major-mode 'erc-mode) t))))
