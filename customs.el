(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-process-type 'cabal-repl)
 '(read-extended-command-predicate #'command-completion-default-include-p)
 '(safe-local-variable-values
   '((eval citar-capf-setup)
     (elisp-lint-indent-specs (describe . 1) (it . 1) (thread-first . 0)
                              (cl-flet . 1) (cl-flet* . 1))
     (eval setq-local hl-todo--regexp (concat "\\[" (hl-todo--regexp) "|"))
     (org-export-initial-scope . buffer)
     (org-tag-alist (:startgroup) ("@phone") ("@pc") (:endgroup)
                    ("ARCHIVE") ("resources") ("projects") ("watch")
                    ("math") ("extras") ("additions") ("references")
                    ("readings") ("movies"))
     (mymy-org-default-export-directory
      . "~/Dropbox (Maestral)/org/artifacts/"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fantasque Sans Mono" :foundry "outline" :slant normal :weight normal :height 120 :width normal))))
 '(olivetti-fringe ((t (:foreground "#353535" :background "#353535")))))
