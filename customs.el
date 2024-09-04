(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.config/emacs/bookmarks")
 '(haskell-process-type 'cabal-repl)
 '(read-extended-command-predicate #'command-completion-default-include-p)
 '(safe-local-variable-values
   '((cov-lcov-file-name . "./test/ProyectoFinal.Domain.Tests/coverage.info")
     (package-lint-main-file . "haskell-mode.el")
     (etags-regen-ignores "test/manual/etags/")
     (etags-regen-regexp-alist
      (("c" "objc")
       "/[ \11]*DEFVAR_[A-Z_ \11(]+\"\\([^\"]+\\)\"/\\1/" "/[ \11]*DEFVAR_[A-Z_ \11(]+\"[^\"]+\",[ \11]\\([A-Za-z0-9_]+\\)/\\1/"))
     (org-tag-alist
      (:startgroup)
      ("@phone")
      ("@pc")
      (:endgroup)
      ("ARCHIVE")
      ("resources")
      ("projects")
      ("watch")
      ("math")
      ("extras")
      ("additions")
      ("references")
      ("readings"))
     (mymy-org-default-export-directory . "~/Dropbox (Maestral)/org/artifacts/"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fantasque Sans Mono" :foundry "outline" :slant normal :weight normal :height 120 :width normal))))
 '(olivetti-fringe ((t (:foreground "#353535" :background "#353535")))))
