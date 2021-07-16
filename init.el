;; Launch the debugger with a stacktrace if someshing goes wrong
;; Goddammit, it can be annoying
;; (setq debug-on-error t)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize) ;; Initialize all the packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(set-frame-parameter nil 'fullscreen 'fullboth) ; Fullscreen
(setq redisplay-dont-pause t)
(setq frame-resize-pixelwise t)
;; (setq gc-cons-threshold 40000000) ; Garbage collector threshold: 40mb
(setq gc-cons-threshold 100000000) ; Garbage collector threshold: 100mb
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(tool-bar-mode -1)   ; This is much easier
(menu-bar-mode -1)   ; than needing to change
(scroll-bar-mode -1) ; this in every OS
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(unless package-archive-contents (package-refresh-contents)) ;; fetch the list of packages available
(setq byte-compile-warnings '(not obsolete));; Cl warnings
(defvar myPackages ;; define list of packages to install
  '(elpy
    use-package
    company-jedi
    company-quickhelp
    pyenv-mode
    elisp-slime-nav
    py-autopep8
    company-anaconda
    anaconda-mode
    pipenv
    ;; poetry
    ;; company-tabine
    golden-ratio
    ryo-modal
    julia-mode
    yasnippet-snippets
    change-inner
    expand-region
    avy
    ace-mc
    helm-swoop
    helm-descbinds
    helm-projectile
    helm-ag
    which-key
    spaceline
    smartparens
    smart-yank
    nyan-mode
    aggressive-indent
    dracula-theme
    ag
    org-bullets
    better-defaults
    multiple-cursors
    slime-company
    rainbow-delimiters
    flycheck
    jupyter
    with-editor
    electric-operator
    direx
    all-the-icons ;; Spaceline
    imenu-list
    magit
    selected
    blacken
    importmagic
    undo-tree
    dashboard
    hide-mode-line
    live-py-mode
    emacsql-sqlite
    emacsql-sqlite3
    bind-key
    yasnippet
    ace-jump-mode
    ace-window
    hydra
    centaur-tabs
    cider
    org-roam
    csv-mode
    pdf-tools
    haskell-mode
    lsp-haskell
    )
  )
(mapc #';; install all packages in list
 (lambda
   (package)
   (unless
       (package-installed-p package)
     (package-install package)))
 myPackages)

(add-to-list 'load-path "~/.emacs.d/elisp/")
;; Load all the keys that don't go to a use-packag
(require 'keys)
;; Load all functions
(require 'functions)
;; Load the maped ryo-modal
(require 'MyMy-mode)

(require 'centered-cursor-mode)

;; (require 'org-protocol)
(require 'org-roam-protocol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;START;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Freely change the font
;; Set the font
(custom-set-faces
 '(default
    ((t
      (:family "Fantasque Sans Mono"
               :foundry "outline"
               :slant normal
               :weight normal
               :height 130
               :width normal
               )))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;MODES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (load-theme 'doom-city-lights t)
(load-theme 'dracula t)
;; (load-theme 'monokai-pro t)
(projectile-mode t)
(show-smartparens-global-mode t)
(smartparens-global-mode t)
(global-hl-line-mode t)
(global-centered-cursor-mode t)
(global-whitespace-mode t)
(golden-ratio-mode t)
(xterm-mouse-mode t)
(savehist-mode t)
(spaceline-emacs-theme t)
(pdf-tools-install)
(save-place-mode)
;; (desktop-save-mode)
;; optional: automatically load previous session on startup
;; (desktop-read)


;; (use-package eaf
;;   :load-path "~/.emacs.d/site-lisp/emacs-application-framework" ; Set to "/usr/share/emacs/site-lisp/eaf" if installed from AUR
;;   :init
;;   (use-package epc :defer t :ensure t)
;;   (use-package ctable :defer t :ensure t)
;;   (use-package deferred :defer t :ensure t)
;;   (use-package s :defer t :ensure t)
;;   :custom
;;   (eaf-browser-continue-where-left-off t)
;;   :config
;;   (eaf-setq eaf-browser-enable-adblocker "true")
;;   (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key take_photo "p" eaf-camera-keybinding)
;;   (eaf-bind-key nil "M-q" eaf-browser-keybinding) ;; unbind, see more in the Wiki
;;   )
(use-package cider
  :ensure t
  :hook
  (clojure-mode . cider-mode)
  )
(use-package haskell-mode
  :ensure t
  :hook
  (haskell-mode . lsp)
  (haskell-literate-mode . lsp)
  )
(use-package clojure-mode
  :ensure t)
(use-package elpy
  :bind (:map python-mode-map
              ("C-c C-q" . jupyter-eval-buffer)
              ("C-c C-j" . jupyter-run-repl)
              ("C-M-y" . company-jedi)
              )
  :init (with-eval-after-load 'python (elpy-enable))
  :hook
  (python-mode . anaconda-mode)
  (python-mode . rainbow-delimiters-mode)
  (python-mode . highlight-indentation-mode)
  (python-mode . electric-operator-mode)
  (python-mode . hs-minor-mode)
  (python-mode . (lambda()
                   (setq-local
                    fill-column 79
                    company-dabbrev-char-regexp "\\sw\\|\\s_"
                    )))
  (elpy-mode . mymy-global-mode)
  (inferior-python-mode . hide-mode-line-mode)
  :config
  (setq python-indent-guess-indent-offset-verbose nil)
  (when (load "flycheck" t t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  ;; (add-hook 'pyvenv-post-activate-hooks (lambda()
  ;;                                         (pyvenv-restart-python)))
  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args "--colors=Linux --profile=default
 --simple-prompt -i"
   python-shell-prompt-regexup "In \\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
   python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
   python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
  )
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "s-m")
  :config
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)
     ))
  (setq lsp-log-io t)
  :hook
  ((python-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration)
   )
  )
(use-package lsp-ui
  :config (setq lsp-ui-sideline-show-hover t
                lsp-ui-sideline-delay 0.5
                lsp-ui-doc-delay 5
                lsp-ui-sideline-ignore-duplicates t
                lsp-ui-doc-position 'bottom
                lsp-ui-doc-alignment 'frame
                lsp-ui-doc-header nil
                lsp-ui-doc-include-signature t
                lsp-ui-doc-use-childframe t)
  :commands lsp-ui-mode)
(use-package company-jedi
  :ensure t
  :hook (python-mode . enable-jedi)
  :config (setq jedi:complete-on-dot t)
  )
(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended)
  )
(use-package blacken
  :hook (python-mode . blacken-mode)
  :config
  (setq blacken-line-length 79))
(use-package company
  :config
  (setq company-idle-delay 1)
  (setq company-dabbrev-downcase nil)
  ;; Show suggestions after entering Nth characters.
  (setq company-minimum-prefix-length 3)
  (setq company-selection-wrap-around t)
  (setq company-require-match nil)
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations t)
  (add-to-list 'company-frontends '(company-pseudo-tooltip-frontend
                                    company-echo-metadata-frontend))
  ;; (add-to-list 'company-backends #'company-tabnine)
  (define-key yas-minor-mode-map "\C-l" 'yas-expand)
  (define-key yas-keymap "\C-l" 'yas-next-field-or-maybe-expand)
  (dolist (keymap (list yas-minor-mode-map yas-keymap))
    (define-key keymap (kbd "TAB") nil)
    (define-key keymap [(tab)] nil))
  (define-key company-active-map (kbd "TAB") 'company-select-next)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-active-map (kbd "RET") nil)
  (global-company-mode)
  (company-tng-mode)
  )
(use-package company-quickhelp
  :ensure t
  :init
  (eval-after-load 'company
    '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))
  :config
  (company-quickhelp-mode)
  )
(use-package flycheck
  :bind (
         :map flycheck-mode-map
         ("M-n" . flycheck-next-error)
         ("M-u" . flycheck-previous-error))
  )
;;;;;                   HELM MODE
(use-package helm
  :ensure t
  :ensure helm-swoop
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x p" . helm-projectile)
         ("C-c h s" . helm-swoop)
         :map helm-map
         ("C-n" . helm-next-line)
         ("C-u" . helm-previous-line)
         )
  :init
  (setq helm-split-window-default-side 'right)
  (setq helm-quick-update t)
  (setq helm-bookmark-show-location t)
  (setq helm-buffers-fuzzy-matching t)
  (setq org-cycle-include-plain-lists 'integrate)
  (setq helm-mini-default-sources '
        (helm-source-buffers-list
         helm-source-recentf
         helm-source-bookmarks
         helm-source-buffer-not-found))
  (spaceline-helm-mode t)
  (spaceline-toggle-helm-number-on)
  (helm-mode)
  )
(use-package magit
  :ensure t)
(use-package ispell
  ;; https://200ok.ch/posts/2020-08-22_setting_up_spell_checking_with_multiple_dictionaries.html
  :config
  (setq ispell-program-name "hunspell")
  ;; Configure German, Swiss German, and two variants of English.
  (setq ispell-dictionary "en_US,es_ES")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,es_ES")
  ;; For saving words to the personal dictionary, don't infer it from
  ;; the locale, otherwise it would save to ~/.hunspell_de_DE.
  (setq ispell-personal-dictionary "~/Dropbox (Maestral)/.hunspell_personal")
  ;; The personal dictionary file has to exist, otherwise hunspell will
  ;; silently not use it.
  (unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0))
  )
(use-package org-mode
  :init
  (setq org-modules '(ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe ol-rmail ol-w3m))
  (setq org-clock-string-limit 25)
  (setq spaceline-org-clock-format-function 'dwim/org-clock-get-string)
  (define-key org-mode-map (kbd "C-j") 'nil)
  (define-key org-mode-map (kbd "C-j") (lambda (count)
					                     (interactive "p")
					                     (insert-char #x30 count)
					                     ))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . nil)
     (latex . t)
     (ledger . t)
     (ocaml . nil)
     (octave . t)
     (python . t)
     (ruby . t)
     (screen . nil)
     (sql . nil)
     (sqlite . t)))
  :config
  (add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
  (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags))
  :custom
  ;; org-default-notes-file "~/task-of-the-day.org"
  ;; org-default-notes-file "~/org-mode/class-note-taking.org"
  ;; initial-buffer-choice  org-default-notes-filev
  ;; Pdflatex
  (org-latex-pdf-process
   '("pdflatex -interaction nonstopmode -output-directory %o %f"
     ;; "bibtex %b"
     "pdflatex -interaction nonstopmode -output-directory %o %f"
     "pdflatex -interaction nonstopmode -output-directory %o %f"))
  ;; Xelatex
  ;; (org-latex-pdf-process
  ;;  '("xelatex -interaction nonstopmode %f"
  ;;    "xelatex -interaction nonstopmode %f"))
  (org-ellipsis "⤵")
  (org-log-done t)
  (org-hide-emphasis-markers t)
  (org-refile-targets '((nil :maxlevel . 2)))
  (org-catch-invisible-edits 'error)
  (org-special-ctrl-a/e t)
  (org-habit-show-all-today t)
  ;; In collapsed view, hide empty lines between subtrees
  (org-cycle-separator-lines 0)
  ;; Theres seems to be a bug where i can't set new emphasis keywords
  ;; So the only way to set one is overwriting one (org-emphasis-alist
  ;; (btw, i can just modify org-font-lock-extra-keywords but i will not
  ;; get (org-hide-emphasis-markers t) with my current knowledge
  (org-emphasis-alist
   (quote (("*" (bold :foreground "magenta"))
           ("/" italic)
           ("_" underline)
           ("=" (underline org-code))
           ("~" org-code verbatim)
           ("+"
            (:strike-through t))
           )))
  (org-pomodoro-length 25)
  (org-pomodoro-short-break-length 5)
  (org-pomodoro-long-break-length 15)
  (org-startup-folded t)
  (org-default-notes-file "~/Dropbox (Maestral)/Creativè/agenda.org")
  (org-agenda-files '("~/Dropbox (Maestral)/Creativè/Notebook/"))
  (org-todo-keyword-faces
   '(("CANCELLED" . (:foreground "red" :weight bold))
     ("CLASS" . (:foreground "purple" :weight bold))
     ))
  (org-todo-keywords
   '((sequence "TODO" "DOING" "|" "DONE")
     (sequence "ASSIGMENT" "|" "ASSIGMENT-DONE")
     (sequence "CLASS" "|")
     (sequence "CANCELLED")))
  (org-capture-templates
   ;; https://orgmode.org/manual/Template-expansion.html#Template-expansion
   ;; https://orgmode.org/manual/Template-elements.html#Template-elements
   '(("q" "Queue")
     ("qt" "Queue Task Waiting" entry (file+headline "~/Dropbox (Maestral)/Creativè/notebook.org" "=What I'm Planing to do=")
      "* %?\n %i" :prepend t
      )
     ("qi" "Queue Task Doing" entry (file+headline "~/Dropbox (Maestral)/Creativè/notebook.org" "=What I'm Doing Now=")
      "* %?\n %i" :clock-in t :prepend t
      )
     ("l" "list")
     ("ls" "list song" entry (file+headline "~/Dropbox (Maestral)/Creativè/notebook.org" "List songs")
      "* [[%x][%?]]"
      )
     ("t" "Simple Todo")
     ("ts" "Todo Scheduled" entry (file+headline "~/Dropbox (Maestral)/Creativè/agenda.org" "Tasks")
      "* TODO %^t %?\n  %i\n"
      )
     ("tf" "Todo Scheduled pointing to a file" entry (file+headline "~/Dropbox (Maestral)/Creativè/agenda.org" "Tasks")
      "* TODO %^t %?\n %i\n %a"
      )
     ("tt" "Plain Todo" entry (file+headline "~/Dropbox (Maestral)/Creativè/agenda.org" "Tasks")
      "* TODO %?\n  %i\n"
      )
     ("td" "Todo Daily" entry (file+headline "~/Dropbox (Maestral)/Creativè/agenda.org" "Daily")
      "* TODO %?\n    SCHEDULED: <%<%Y-%m-%d %a> ++1d/2d>\n    :PROPERTIES:\n    :STYLE:    habit\n    :END:\n"
      )
     ("k" "Japanese")
     ("kk" "Kanji" entry (file+headline "~/Dropbox (Maestral)/Creativè/Japanese/kanji.org" "Kanji Bin")
      "* %?\n#TODO\nMeaning: \nKun: \nOn: \n"
      )
     ("kw" "Word" entry (file+headline "~/Dropbox (Maestral)/Creativè/Japanese/words.org" "Word Bin")
      "* %?\n#TODO\nMeaning: \nReading: \nType: "
      )
     ))
  (org-format-latex-options '(plist-put org-format-latex-options :scale 2.0 :background auto :foreground "white"))
  (org-highlight-latex-and-related '(latex script entities))
  :bind (("C-c o c" . org-capture))
  :hook
  (org-mode . org-superstar-mode)
  (org-mode . prettify-symbols-mode)
  (org-mode . org-indent-mode)
  (org-mode . flyspell-mode)
  (org-mode . auto-fill-mode)
  ;; (org-mode . electric-operator-mode)
  (org-mode
   . (lambda () (setq-local tab-width 2
                            indent-tabs-mode nil
                            python-shell-interpreter "python3"
                            )))
  )
;; Why do i live, just to suffer?
(use-package org-journal
  :ensure t
  :defer t
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j")
  :config
  (setq org-journal-dir "~/Dropbox (Maestral)/Creativè/journal/"
        org-journal-date-format "%A, %d %B %Y")
  )
(use-package org-superstar
  :ensure t
  :config
  (org-superstar-configure-like-org-bullets)
  (setq org-superstar-headline-bullets-list '(?◉ ?⭆ ?○ ?✸ ?✿ ?✥ ?❂ ?❄ ?⁋))
  )
(use-package org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(use-package org-roam
  :ensure t
  :config
  (add-hook 'org-open-at-point-functions
            #'org-roam-open-at-point nil)
  :hook
  (after-init . org-roam-mode)
  (after-init . winner-mode)
  (org-mode . nroam-mode)
  :custom
  (org-roam-graph-viewer "/usr/bin/google-chrome-stable")
  (org-roam-graph-exclude-matcher '("daily"))
  (org-roam-graph-node-extra-config
   '(("color" . "skyblue")))
  (org-roam-completion-everywhere t)
  (org-roam-buffer-window-parameters '((no-delete-other-windows . t)))
  ;; Daily notes
  (org-roam-dailies-directory "capture")
  (org-roam-directory "~/Dropbox (Maestral)/Creativè/org-roam/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      #'org-roam-capture--get-point
      "* %?"
      :file-name "capture/capture"
      :head "#+title: Note\n\n"
      :olp ("Default note"))
     ("f" "Findings" entry
      #'org-roam-capture--get-point
      "* %?\n"
      :file-name "capture/capture"
      :head "#+title: Note\n\n"
      :olp ("Findings")
      )
     ("s" "Code Snippet")
     ("sp" "Python" entry
      #'org-roam-capture--get-point
      "* %?\n#+BEGIN_SRC python\n%x\n#+END_SRC"
      :file-name "capture/capture"
      :head "#+title: Note\n\n"
      :olp ("Code Snippet")
      )
     ("c" "Programming")
     ("cc" "Python" entry
      #'org-roam-capture--get-point
      "* %?\n"
      :file-name "capture/capture"
      :head "#+title: Note\n\n"
      :olp ("Code General"))
     ("t" "Magnum Opus" entry
      #'org-roam-capture--get-point
      "* %?\n"
      :file-name "capture/capture"
      :head "#+title: Note\n\n"
      :olp ("Magnum Opus")
      )
     ("l" "Links" entry
      #'org-roam-capture--get-point
      "* [[%x][%?]]\n"
      :file-name "capture/capture"
      :head "#+title: Note\n\n"
      :olp ("Links")
      )
     ("o" "Ocurrence" entry
      #'org-roam-capture--get-point
      "* %?\n"
      :file-name "capture/capture"
      :head "#+title: Note\n\n"
      :olp ("Ocurrence")
      )
     ("p" "Phone" entry
      #'org-roam-capture--get-point
      "* %x \n"
      :file-name "capture/capture"
      :head "#+title: Note\n\n"
      :olp ("Phone")
      )
     ("y" "Lists" entry
      #'org-roam-capture--get-point
      "* %? \n"
      :file-name "capture/capture"
      :head "#+title: Note\n\n"
      :olp ("Lists")
      )
     ("j" "Japanese")
     ("jk" "Japanese Knowledge" entry ;; This include but not limited to: grammar, use case
      #'org-roam-capture--get-point
      "* Knowledge\n%?\n"
      :file-name "capture/capture"
      :head "#+title: Note\n\n"
      :olp ("Japanese")
      )
     ("js" "Japanese Sentences" entry
      #'org-roam-capture--get-point
      "* Sentence\n%?\n"
      :file-name "capture/capture"
      :head "#+title: Note\n\n"
      :olp ("Japanese")
      )
     ))
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph)
               ("C-c n c" . org-roam-dailies-capture-today)
               ;; ("C-c n ")
               )
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))
              )
  )
(use-package deft
  :ensure t
  :custom
  (deft-extensions '("org"))
  (deft-directory "~/Dropbox (Maestral)/Creativè/org-roam/")
  (deft-recursive t)
  )
(use-package anki-editor
  :ensure t
  :config
  (setq anki-editor--ox-anki-html-backend
        (if anki-editor-use-math-jax
            (org-export-create-backend
             :parent 'html
             :transcoders '((latex-fragment . anki-editor--ox-latex-for-mathjax)
                            (latex-environment . anki-editor--ox-latex-for-mathjax)
                            (paragraph . strip-<p>-html)
                            ))
          (org-export-create-backend
           :parent 'html
           :transcoders '((latex-fragment . anki-editor--ox-latex)
                          (latex-environment . anki-editor--ox-latex)
                          (paragraph . strip-<p>-html)
                          ))))
  )
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind (("C-c p s" . 'projectile-switch-project)
         )
  :init
  (setq-default projectile-project-search-path '("~/Projects/"))
  (setq projectile-completion-system 'helm
        projectile-switch-project-action 'helm-projectile)
  )
(use-package spaceline
  :ensure t
  :init
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-buffer-encoding-off)
  (spaceline-toggle-buffer-encoding-abbrev-off)
  (spaceline-toggle-buffer-modified-off)
  (spaceline-toggle-hud-off)
  (spaceline-toggle-org-pomodoro-off)
  (spaceline-toggle-input-method-off)
  (spaceline-toggle-line-column-off)
  (spaceline-toggle-org-clock-on)
  (spaceline-toggle-which-function-on)
  (spaceline-toggle-projectile-root-on)
  (spaceline-toggle-buffer-size-on)
  (spaceline-toggle-python-pyvenv-on)
  (spaceline-toggle-process-on)
  (spaceline-toggle-nyan-cat-on)
  (setq powerline-default-separator 'wave)
  (spaceline-compile)
  )
(use-package avy
  :ensure t
  :bind(("M-g g" . avy-goto-line)
        ("M-g M-g" . avy-goto-line)
        )
  :init
  (setq avy-case-fold-search nil)
  (setq avy-keys '(?n ?e ?i ?k ?y ?m ?u ?c ?r ?s ?t))
  )
(use-package ace-window
  :ensure t
  :custom
  (aw-keys '(?n ?e ?i ?k ?y ?m ?u)))

;;; Minor packages
(use-package dashboard
  :ensure t
  :init
  ;; It's just that i like the phrase
  (setq dashboard-banner-logo-title "Welcome back, legend 800 Japanese words await you")
  ;; Set the banner
  ;; (setq dashboard-startup-banner [VALUE])
  (setq dashboard-set-footer nil)
  (setq dashboard-show-shortcuts t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 10)
                          (projects . 5)
                          ))
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  )
(use-package undo-tree
  :ryo
  ("q /" undo-tree-visualize)
  :bind
  (:map undo-tree-visualizer-mode-map
        ("N" . undo-tree-visualize-switch-branch-left)
        ("n" . undo-tree-visualize-redo)
        ("u" . undo-tree-visualize-undo)
        ("U" . undo-tree-visualize-switch-branch-right)
        ))
(use-package selected ;; Special keybindings when a region is active
  :ensure t
  ;; selected-<major-mode>-map for major mode specific keybindings
  ;; (setq selected-org-mode-map (make-sparse-keymap)) example org mode
  :commands selected-minor-mode
  :config
  (setq selected-minor-mode-override t)
  :bind (:map selected-keymap
              ("q" . selected-off)
              ("w" . count-words-region))
  )
(use-package nyan-mode ;; Nyan, simple nyan
  :ensure t
  :diminish nyan-mode
  :init
  (setq nyan-animate-nyancat t)
  (setq nyan-wavy-trail t)
  :hook
  ((after-init) . nyan-mode)
  )
(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-change-fonts "Fantasque Sans Mono" 160)
  (setq centaur-tabs-style "bar"
        centaur-tabs-set-bar 'over
        centaur-tabs-set-icons t
        centaur-tabs-set-close-button nil
        centaur-tabs-cycle-scope 'tabs
        centaur-tabs-show-new-tab-button nil
        centaur-tabs-label-fixed-length 12
        )
  (setq uniquify-separator "/"
        uniquify-buffer-name-style 'forward)
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

 Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
 All buffer name start with * will group to \"Emacs\".
 Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ;; ((not (eq (file-remote-p (buffer-file-name)) nil))
      ;; "Remote")
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode
                              )))
       "Emacs")
      ((or (derived-mode-p 'prog-mode)
           (memq major-mode '(org-mode
                              org-agenda-clockreport-mode
                              org-src-mode
                              org-agenda-mode
                              org-beamer-mode
                              org-indent-mode
                              org-bullets-mode
                              org-cdlatex-mode
                              org-agenda-log-mode
                              diary-mode)))
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
                          help-mode))
       "Help")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  :bind
  ("C-S-<iso-lefttab>" . centaur-tabs-backward)
  ("C-<tab>" . centaur-tabs-forward)
  ("C-z" . centaur-tabs-backward-group)
  ("C-S-z" . centaur-tabs-forward-group)
  )
;; (use-package anki
;;   :defer t
;;   :load-path "~/.emacs.d/lisp/anki/"
;;   :init
;;   (add-hook 'anki-mode-hook #'shrface-mode)
;;   (add-hook 'anki-card-mode-hook #'shrface-mode)
;;   (autoload 'anki "anki")
;;   (autoload 'anki-browser "anki")
;;   (autoload 'anki-list-decks "anki")
;;   :config
;;   ;; (require 'shrface) ; If you use shrface, require it here
;;   (setq anki-shr-rendering-functions (append anki-shr-rendering-functions shr-external-rendering-functions))
;;   (setq sql-sqlite-program "/usr/bin/sqlite3")
;;   ;; Set up the collection directory, which should contain a file - collection.anki2 and a folder - collection.media
;;   (setq anki-collection-dir "/Users/chandamon/Library/Application Support/Anki2/User 1")
;;   )
(use-package which-key ;; Useful to tell what is the next command that i can do
  :ensure t
  :init
  (setq which-key-enable-extended-define-key t)
  (setq which-key-side-window-location 'bottom)
  (setq which-key-side-window-max-height 0.40)
  (setq which-key-side-window-max-width 0.66)
  :config
  (push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist)
  (which-key-mode)
  )
(use-package expand-region
  :ensure t
  :bind (("C-'" . er/expand-region)
         )
  )
(use-package change-inner
  :ensure t)
(use-package yequake
  :ensure t
  :custom
  (yequake-frames
   '(("org-roam-dailies-capture-today"
      (buffer-fns . (yequake-org-roam-dailies-capture-today))
      (width . 0.75)
      (height . 0.5)
      (alpha . 0.80)
      (frame-parameters . ((undecorated . t)
                           (skip-taskbar . t)
                           (sticky . t))))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;DEFAULT;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq default-fill-column 74)
(add-to-list 'golden-ratio-extra-commands 'ace-window)
(setq highlight-nonselected-windows t)
(setq use-dialog-box nil) ; Text-based options are better
(setq bidi-display-reordering nil)
(setq default-tab-width 4)
(setq tab-width 4)
(setq inhibit-compacting-font-caches t)
(setq custom-file null-device)
(setq use-package-always-demand (daemonp))
(setq mouse-yank-at-point t)
(setq initial-major-mode 'org-mode)
(setq comp-async-report-warnings-errors nil)
(setq initial-scratch-message "\
# This buffer is for notes you don't want to save, and for Python code.")
(setq default-input-method "japanese-mozc")
(setq-default tab-always-indent 'complete)
(setq-default whitespace-line-column 1000)
(setq-default cursor-type '(hbar . 3)) ;; Change cursor to a bar
(setq-default cursor-in-non-selected-windows nil)
(setq-default blink-cursor-blinks 0) ; Never stop to blink
(setq-default frame-title-format "%b %& emacs")
(setq-default garbage-collection-messages t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default
 prettify-symbols-alist
 '(("#+BEGIN_SRC"     . "Λ")
   ("#+END_SRC"       . "Λ")
   ("#+begin_src"     . "Λ")
   ("#+end_src"       . "Λ")
   ("lambda"          . "λ")
   ("[ ]" . "☐")
   ("[X]" . "☑")
   ("[-]" . "❍")
   ))
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
(setq auto-save-default nil)
(setq sentence-end-double-space nil)
(setq sgml-quick-keys 'close)
(set-frame-parameter (selected-frame) 'buffer-predicate
                     (lambda (buf) (not (string-match-p "^*" (buffer-name buf)))))

;; Treat clipboard input as UTF-8 string first; compound text next, etc. Why?
;; .. match default encoding which is UTF-8 as well.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Don't group undo steps. Why?
;; .. without this is groups actions into a fixed number of
;;    steps which feels unpredictable.
(fset 'undo-auto-amalgamate 'ignore)

;; Limit of 64mb.
(setq undo-limit 6710886400)
;; Strong limit of 1.5x (96mb)
(setq undo-strong-limit 100663296)
;; Outer limit of 10x (960mb).
;; Note that the default is x100), but this seems too high.
(setq undo-outer-limit 1006632960)

(setq exec-path (append exec-path '("/home/frailin/.local/bin")))
(setq jit-lock-defer-time 0)
(setq fast-but-imprecise-scrolling t)
(setq auto-window-vscroll nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq savehist-additional-variables '(register-alist))
(setq
 split-height-threshold 4
 split-width-threshold 40
 split-window-preferred-function 'split-window-really-sensibly)
;;;;Terminal

;; use xclip to copy/paste in emacs-nox
(unless window-system
  (when (getenv "DISPLAY")
    (defun xclip-cut-function (text &optional push)
      (with-temp-buffer
        (insert text)
        (call-process-region
         (point-min) (point-max)
         "xclip" nil 0 nil "-i" "-selection" "clipboard")))
    (defun xclip-paste-function()
      (let ((xclip-output
             (shell-command-to-string "xclip -o -selection clipboard")))
        (unless (string= (car kill-ring) xclip-output)
          xclip-output )))
    (setq interprogram-cut-function 'xclip-cut-function)
    (setq interprogram-paste-function 'xclip-paste-function)
    ))

(spaceline-define-segment ryo-state
  (when (bound-and-true-p ryo-modal-mode)
    "--MODAL--")
  )

;;;;;                   Don't know where to put it, but it's still config
(with-eval-after-load "linum"
  ;; set `linum-delay' so that linum uses `linum-schedule' to update linums.
  (setq linum-delay t)
  ;; create a new var to keep track of the current update timer.
  (defvar-local my-linum-current-timer nil)
  ;; rewrite linum-schedule so it waits for 1 second of idle time
  ;; before updating, and so it only keeps one active idle timer going
  (defun linum-schedule ()
    (when (timerp my-linum-current-timer)
      (cancel-timer my-linum-current-timer))
    (setq my-linum-current-timer
          (run-with-idle-timer 1 nil #'linum-update-current))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;Pretify;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom--inhibit-theme-enable nil)
(set-face-attribute 'region nil :background "#a3b32e")
(with-eval-after-load 'avy
  (set-face-attribute 'avy-lead-face nil
                       :background "#818182" :foreground "#000000")
  (set-face-attribute 'avy-lead-face-0 nil
                       :background "#bdbca6" :foreground "#000000")
  )


(defface font-lock-operator-face
  '((t :foreground "#8fff9e")) "Basic face for operator."
  :group 'basic-faces)
(defface font-lock-number-face
  '((t :foreground "#f56356")) "Basic face for number."
  :group 'basic-faces)

(custom-theme-set-faces
 'dracula
 '(font-lock-variable-name-face ((t (:foreground "#3fa4e8" :bold t))) t)
 )

(dolist (mode-iter '(python-mode))
  (font-lock-add-keywords mode-iter
                          '(("\\([@~^&\|!<>:=\\+*/%-]\\)" 0
                             'font-lock-operator-face keep)
                            ("\\<[\\+-]?[0-9]+\\(.[0-9]+\\)?\\>" 0
                             'font-lock-number-face keep)
                            )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;HOOKS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; (add-hook 'window-configuration-change-hook
;;           (lambda ()
;;             (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) 2 2)))

(add-hook 'emacs-lisp-mode-hook
          (lambda()
            (aggressive-indent-mode)
            (company-mode)
            (rainbow-delimiters-mode)
            ))

(add-hook 'csv-mode-hook
          'csv-align-mode
          )

(add-hook 'post-command-hook ;Execute after a command
          'hcz-set-cursor-color-according-to-mode)
(add-hook 'after-init-hook
          (lambda()
            (which-function-mode t)
            (mymy-global-mode t)
            (yas-global-mode t)
            (global-undo-tree-mode)
            ))
(add-hook 'prog-mode
          (lambda()
            (auto-fill-mode t)
            (rainbow-delimiters-mode t)
            )
          )
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)
(add-hook 'java-mode-hook #'lsp)
(define-minor-mode mymy-mode
  "Define all keys to have a preference to override others"
  :init-value nil
  :lighter " mymy"
  :keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "S-SPC") 'ryo-modal-global-mode)
    (define-key keymap (kbd "M-t") 'ryo-modal-global-mode)
    keymap)
  :group 'mymy)
(define-globalized-minor-mode mymy-global-mode mymy-mode
  (lambda ()
    (if (not (minibufferp (current-buffer)))
        (mymy-mode t))))
(define-globalized-minor-mode ryo-modal-global-mode ryo-modal-mode
  (lambda ()
    (if (not (minibufferp (current-buffer)))
        (ryo-modal-mode t))))

;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt"
;;       python-shell-prompt-detect-failure-warning nil)
;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;;              "jupyter")








































(ryo-modal-global-mode t)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
;; Init.el ends here
