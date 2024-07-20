;; Disable package.el, the default package manager of emacs
(setq package-enable-at-startup nil)

;; Setup custom file for auto-generated configuration from emacs
(setq custom-file (expand-file-name "customs.el" user-emacs-directory))

