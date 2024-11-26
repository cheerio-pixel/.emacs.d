(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.config/emacs/bookmarks")
 '(haskell-process-type 'cabal-repl)
 '(read-extended-command-predicate #'command-completion-default-include-p)
 '(safe-local-variable-values
   '((eval spec-keywords 1)
     (elisp-lint-indent-specs
      (describe . 1)
      (it . 1)
      (thread-first . 0)
      (cl-flet . 1)
      (cl-flet* . 1)
      (org-element-map . defun)
      (org-roam-dolist-with-progress . 2)
      (org-roam-with-temp-buffer . 1)
      (org-with-point-at . 1)
      (magit-insert-section . defun)
      (magit-section-case . 0)
      (org-roam-with-file . 2))
     (elisp-lint-ignored-validators "byte-compile" "package-lint")
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
      ("readings")
      ("movies"))
     (eval setq-local hl-todo--regexp
           (concat "\\["
                   (hl-todo--regexp)
                   "|"))
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
     (org-export-initial-scope . buffer)
     (mymy-org-default-export-directory . "~/Dropbox (Maestral)/org/artifacts/"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(default ((t (:family "Fantasque Sans Mono" :foundry "outline" :slant normal :weight normal :height 130 :width normal))))
 ;; '(default ((t (:family "Fantasque Sans Mono" :foundry "outline" :slant normal :weight normal :height 120 :width normal))))
 `(default ((t (:family "Fantasque Sans Mono" :foundry "outline" :slant normal :weight normal
                        :height ,(if (string= "tic12" (system-name))
                                     160
                                   120)
                        :width normal))))
 '(olivetti-fringe ((t (:foreground "#353535" :background "#353535")))))