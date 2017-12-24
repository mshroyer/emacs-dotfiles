;; (require 'epa-file)
;; (epa-file-enable)
;; (setq epg-debug t)

(setq mm-decrypt-option 'always
      mm-verify-option 'always
      mml-smime-use 'epg
      gnus-buttonized-mime-types '("multipart/encrypted" "multipart/signed")

      gnus-save-newsrc-file nil
      gnus-read-newsrc-file nil)

(setq gnus-parameters
      '((".*"
         (gcc-self . none))))

;; Inspired by https://www.emacswiki.org/emacs/GnusFormatting
(setq-default gnus-group-line-format "%M%S%p%P%8y:%B%(%g%)\n"
              gnus-summary-line-format "%U%R%z %(%&user-date;  %-25,25f  %B%s%)\n"
              gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
              gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
              gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
              gnus-sum-thread-tree-false-root ""
              gnus-sum-thread-tree-indent " "
              gnus-sum-thread-tree-leaf-with-other "├► "
              gnus-sum-thread-tree-root ""
              gnus-sum-thread-tree-single-leaf "╰► "
              gnus-sum-thread-tree-vertical "│")
