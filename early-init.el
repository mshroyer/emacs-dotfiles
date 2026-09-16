;; Work around -mmacosx-version-min=18.0 compilation errors
(when (eq system-type 'darwin)
  (setenv "MACOSX_DEPLOYMENT_TARGET"
          (string-trim (shell-command-to-string "sw_vers -productVersion"))))

