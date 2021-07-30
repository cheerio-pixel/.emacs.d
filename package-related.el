;; (add-to-list 'package-archives
;;              '("elpy" . "http://jorgenschaefer.github.io/packages/"))
;; (require 'package) package.el related
;; package.el related
;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/") t)
;; package.el related
;; (package-initialize) ;; Initialize all the packages
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package)
;;   )
;; package.el related
;; (unless package-archive-contents (package-refresh-contents)) ;; fetch the list of packages available
;; (defvar myPackages ;; define list of packages to install
;;   '(elpy
;;     use-package
;;     company-jedi
;;     company-quickhelp
;;     pyenv-mode
;;     elisp-slime-nav
;;     py-autopep8
;;     company-anaconda
;;     anaconda-mode
;;     pipenv
;;     ;; poetry
;;     ;; company-tabine
;;     golden-ratio
;;     ryo-modal
;;     julia-mode
;;     yasnippet-snippets
;;     change-inner
;;     expand-region
;;     avy
;;     ace-mc
;;     helm-swoop
;;     helm-descbinds
;;     helm-projectile
;;     helm-ag
;;     which-key
;;     spaceline
;;     smartparens
;;     smart-yank
;;     nyan-mode
;;     aggressive-indent
;;     dracula-theme
;;     ag
;;     org-bullets
;;     better-defaults
;;     multiple-cursors
;;     slime-company
;;     rainbow-delimiters
;;     flycheck
;;     jupyter
;;     with-editor
;;     electric-operator
;;     direx
;;     all-the-icons ;; Spaceline
;;     imenu-list
;;     magit
;;     selected
;;     blacken
;;     importmagic
;;     undo-tree
;;     dashboard
;;     hide-mode-line
;;     live-py-mode
;;     emacsql-sqlite
;;     emacsql-sqlite3
;;     bind-key
;;     yasnippet
;;     ace-jump-mode
;;     ace-window
;;     hydra
;;     centaur-tabs
;;     cider
;;     org-roam
;;     csv-mode
;;     pdf-tools
;;     haskell-mode
;;     lsp-haskell
;;     )
;;   )
;; package.el related
;; (mapc #';; install all packages in list
;;  (lambda
;;    (package)
;;    (unless
;;        (package-installed-p package)
;;      (package-install package)))
;;  myPackages)
