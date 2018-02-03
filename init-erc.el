(require 'erc)
(add-hook 'erc-mode-hook
          (lambda ()
            (make-local-variable 'scroll-margin)
            (setq scroll-margin 0)))

;; Keep the prompt at the bottom of the window
(erc-scrolltobottom-mode 1)

;; Don't show join/part messages.
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; Better completion.
(setq erc-pcomplete-nick-postfix ": ")

;; Only add ERC channels to the modeline when your nick is mentioned (taken
;; from http://www.emacswiki.org/emacs/ErcChannelTracking)
(setq erc-format-query-as-channel-p t
      erc-track-priority-faces-only 'all
      ;erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE" "333" "353")
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
