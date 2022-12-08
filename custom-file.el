(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("9584533e7ca091a59f88e7f2acc2f8ce9124753d7b82aad6d4526ccf77787975" default))
 '(org-agenda-files
   '("~/Dropbox (Maestral)/Creativè/Notes/2022-01-09-12-38-23-zettelkasten.org" "/home/cheerio-pixel/Dropbox (Maestral)/Creativè/Notes/Projects/20210715113548-projects.org"))
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-w3m org-checklist))
 '(safe-local-variable-values
   '((elisp-lint-indent-specs
      (if-let* . 2)
      (when-let* . 1)
      (let* . defun)
      (nrepl-dbind-response . 2)
      (cider-save-marker . 1)
      (cider-propertize-region . 1)
      (cider-map-repls . 1)
      (cider--jack-in . 1)
      (cider--make-result-overlay . 1)
      (insert-label . defun)
      (insert-align-label . defun)
      (insert-rect . defun)
      (cl-defun . 2)
      (with-parsed-tramp-file-name . 2)
      (thread-first . 1)
      (thread-last . 1))
     (checkdoc-package-keywords-flag)
     (org-roam-db-update-on-save)
     (haskell-compiler-type quote stack)
     (haskell-process-type quote stack)
     (lsp-haskell-server-path . "stack exec -- haskell-language-server-wrapper --lsp -d -l /tmp/hls.log")
     (projectile-project-run-cmd . "mvn compile && java -cp target/classes com.cheerio_pixel.temperature_converter.App")
     (eval when
           (featurep 'flycheck)
           (flycheck-mode -1))
     (org-roam-db-autosync-mode)
     (elisp-lint-indent-specs
      (when-let . 1))
     (eval progn
           (pp-buffer)
           (indent-buffer))
     (eval add-to-list 'load-path
           (file-name-directory
            (buffer-file-name)))
     (elisp-lint-indent-specs
      (describe . 1)
      (it . 1)
      (org-element-map . defun)
      (org-roam-dolist-with-progress . 2)
      (org-roam-with-temp-buffer . 1)
      (org-with-point-at . 1)
      (magit-insert-section . defun)
      (magit-section-case . 0)
      (org-roam-with-file . 2))
     (elisp-lint-ignored-validators "byte-compile" "package-lint")))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fantasque Sans Mono" :foundry "outline" :slant normal :weight normal :height 120 :width normal)))))
