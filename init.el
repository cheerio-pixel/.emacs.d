;; -*- lexical-binding: t; -*-
;;* Straight bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;;* Straight config
(setq straight-use-package-by-default t)
(setq straight-host-usernames '((github . "cheerio-pixel")))
;;* vc-follows-symlinks
(setq vc-follow-symlinks t)
;;* Use package installation
(straight-use-package 'use-package)
(eval-when-compile (require 'use-package))
;;* Require
(require 'cl-lib)
;;* Important packages
;; This packages need to be loaded before all the others
(use-package ryo-modal
  :config
  (define-globalized-minor-mode ryo-modal-global-mode ryo-modal-mode
    (lambda ()
      (if (not (minibufferp (current-buffer)))
          (ryo-modal-mode t)))))

(use-package general)

(use-package el-patch)

;;* Configs
(set-frame-parameter nil 'fullscreen 'fullboth) ; Fullscreen
(setq redisplay-dont-pause t)
(setq frame-resize-pixelwise t)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(tool-bar-mode -1)   ; This is much easier
(menu-bar-mode -1)   ; than needing to change
(scroll-bar-mode -1) ; this on every OS
(setq byte-compile-warnings '(not obsolete));; Cl warnings

;;* Modes
(global-hl-line-mode)
(global-whitespace-mode)
(xterm-mouse-mode)
(savehist-mode)
(save-place-mode)
;;* Alias
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'gsetq 'general-setq)
;;* GC and minibuffer
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 100000000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)
;;* My variables
(setq dropbox-dir "~/Dropbox (Maestral)/")
(setq main-dropbox-dir (concat dropbox-dir "Creativè/"))
(setq mymy-org-roam-dir (concat main-dropbox-dir "Notes/"))
;;* Load path
(add-to-list 'load-path "~/.emacs.d/elisp/")
;;* Keys

;; Same as global-set-key
;;** Unbind
(general-unbind
  "C-z"
  "M-z"
  "C-S-t"
  "C-t"
  "C-v"
  "M-v"
  "C-b"
  "C-f"
  "M-u"
  "C-M-p"
  "C-M-f"
  "C-c C-b"
  "C-d"
  )
;;** global-set
(general-define-key
 "C-M-y" 'duplicate-current-line
 "C-S-e" 'forward-word
 "C-q" 'backward-delete-word
 "C-o" 'smart-open-line
 "S-C-<left>" 'shrink-window-horizontally
 "S-C-<right>" 'enlarge-window-horizontally
 "S-C-<down>" 'shrink-window
 "S-C-<up>" 'enlarge-window
 "C-r" 'backward-char
 "C-s" 'forward-char
 "M-r" 'backward-word
 "M-s" 'forward-to-word
 "C-M-s" 'forward-sexp
 "C-M-r" 'backward-sexp
 "C-M-u" 'backward-list
 "M-b" 'isearch-backward
 "M-f" 'isearch-forward
 "C-M-b" 'isearch-backward-regexp
 "C-q" 'backward-delete-word
 "M-z" 'delete-region
 "M-d" 'delete-word-or-whitespace
 "C->" 'mc/mark-next-like-this
 "C-<" 'mc/mark-previous-like-this
 "C-c m c" 'mc/edit-lines
 "C-a" 'smarter-move-beginning-of-line
 "C-M-n" 'company-capf
 "C-M-e" 'company-complete
 "M-<return>" 'new-line-dwim
 "M-e" 'hippie-expand
 "M-n" 'dabbrev-expand
 "C-c s u" 'straight-use-package
 "C-c s g" 'straight-get-recipe
 "C-;" 'iedit-mode
 )
;;** dired-mode-map
(general-define-key
 :keymaps 'dired-mode-map
 "RET" 'dired-find-alternate-file
 "M-RET" 'dired-find-file
 )
;;** centered-cursor-keymap
(general-define-key
 :keymaps 'centered-cursor-keymap
 "C-M--" 'centered-cursor-raise-position-manually
 "C-M-+" 'centered-cursor-lower-position-manually
 "C-M-=" 'centered-cursor-lower-position-manually
 "C-M-0" 'centered-cursor-reset-position-manually
 )
;;** key-translation-map
(general-define-key
 :keymaps 'key-translation-map
 "C-p" "C-u"
 "C-u" "C-p"
 )
;; (define-key key-translation-map (kbd "C-p") (kbd "C-u"))
;; (define-key key-translation-map (kbd "C-u") (kbd "C-p"))
;;** smartparens-mode-map
(general-define-key
 :keymaps 'smartparens-mode-map
 "C-M-e" 'company-complete
 "M-(" 'sp-wrap-round
 "M-s" 'sp-splice-sexp
 "M-r" 'sp-splice-sexp-killing-around
 )
;;** lispy-mode-map
(general-define-key
 :keymaps 'lispy-mode-map
 "e" 'special-lispy-different
 )
;;** Ryo-modal through general
(general-define-key
 :keymaps 'ryo-modal-mode-map
 :prefix "S"
 "b" 'calibredb-find-helm
 )
;;** python-mode-map
(general-define-key
 :keymaps 'python-mode-map
 "C-c C-d" nil
 )

;;* Set the font
(custom-set-faces
 '(default
    ((t
      (:family "Fantasque Sans Mono"
               :foundry "outline"
               :slant normal
               :weight normal
               :height 130
               :width normal)))))
;;* Emacs theme
(use-package dracula-theme
  :config
  (load-theme 'dracula t)
  (custom-theme-set-faces
   'dracula
   '(font-lock-variable-name-face ((t (:foreground "#3fa4e8" :bold t))) t))
  (custom-theme-set-faces
   'dracula
   '(font-lock-comment-face ((t (:foreground "light pink"))) t))
  (custom-theme-set-faces
   'dracula
   '(font-lock-doc-face ((t (:foreground "spring green"))) t))
  (set-face-attribute 'region nil :background "#a3b32e")
  )
;;* Normal Packages
;;** Niceties
(use-package hydra)

(use-package electric-operator)
(use-package highlight-indentation)
(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))

(use-package ag)
;; sp-pair is not suitiable when you have strict-mode activate
(use-package wrap-region
  :straight (:type git :host github :repo "cheerio-pixel/wrap-region.el")
  :config
  ;; org mode
  (wrap-region-add-wrapper "*" "*" nil 'org-mode)
  (wrap-region-add-wrapper "$" "$" nil 'org-mode)
  ;; latex
  (wrap-region-add-wrapper "\\sqrt{" "}" "s" 'org-mode)
  (wrap-region-add-wrapper "\\frac{" "}{}" "f" 'org-mode)
  (wrap-region-add-wrapper "(" ")^" "^" 'org-mode)
  ;; python
  (wrap-region-add-wrapper "print(" ")" "p" 'python-mode)
  (wrap-region-global-mode t)
  :hook (after-init . wrap-region-global-mode))

(use-package aggressive-indent
  :hook
  (emacs-lisp-mode . aggressive-indent-mode)
  (clojure-mode . aggressive-indent-mode))

(use-package goto-chg
  :config
  (ryo-modal-keys
   ("g;" goto-last-change)
   ("G:" goto-last-change-reverse)))

;;** Web programming
;; Not using for now
;; (use-package web-mode)
;; (use-package impatient-mode)

(use-package centered-cursor-mode
  :straight (:type git :host github :repo "andre-r/centered-cursor-mode.el" :branch "dev")
  :config
  ;; (global-centered-cursor-mode)
  (setq scroll-preserve-screen-position t
        scroll-conservatively 0
        maximum-scroll-margin 0.5
        scroll-margin 99999)
  )

(use-package smartparens
  :init
  (defun mymy/smartparens-hook ()
    (smartparens-global-mode)
    (show-smartparens-global-mode)
    (smartparens-global-strict-mode))
  (setq mymy-lisp-modes '(emacs-lisp-mode clojure-mode cider-mode slime-mode lisp-mode))
  :config (sp-local-pair mymy-lisp-modes "'" "'" :actions nil)
  :hook ((after-init . mymy/smartparens-hook)))

(use-package golden-ratio
  :config (golden-ratio-mode t)
  (add-to-list 'golden-ratio-extra-commands 'ace-window))

;;** Spaceline

(use-package spaceline
  :init
  (spaceline-emacs-theme)
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
  (spaceline-compile))
;;** PDF
(use-package pdf-tools
  :ryo
  (:mode 'pdf-view-mode)
  ("n" pdf-view-next-line-or-next-page)
  ("u" pdf-view-previous-line-or-previous-page)
  ("N" pdf-view-next-page)
  ("U" pdf-view-previous-page)
  :bind (:map pdf-view-mode-map ("C-x l" . pdf-get-page))
  :init
  (defun pdf-get-page ()
    (interactive)
    (message (concat "Page: " (int-to-string (image-mode-window-get 'page)))))
  ;; Don't make the daemon fail when a new version is avalible
  (ignore-errors (pdf-tools-install))
  :config
  ;; It simply needs to exist to extend saveplace
  (use-package saveplace-pdf-view)
  :hook
  (pdf-view-mode . pdf-isearch-minor-mode))

(use-package org-noter
  ;; Someday i will integrate this with org roam or not
  :disabled
  :config
  (setq org-noter-notes-window-location 'other-frame)
  (setq org-noter-always-create-frame nil)
  (setq org-noter-notes-search-path '("~/Sync/Notes/"))
  (setq org-noter-default-notes-file-names nil)
  :ryo
  (:mode 'pdf-view-mode)
  ("i" org-noter-insert-note)
  ("I" org-noter-insert-precise-note)
  )
;;** Vterm
(use-package vterm
  ;; I prefer to open a terminal before opening this
  ;; Good package though
  :disabled
  :bind
  (("C-x l" . vterm))
  :config
  (defun mymy/vterm-hook ()
    (centered-cursor-mode -1))
  :hook
  ((vterm-mode . mymy/vterm-hook))
  )

;;** calibredb
(use-package calibredb
  :defer t
  :config
  ;;; Moved to the sections keys of ryo modal
  ;; (ryo-modal-key "Sb" 'calibredb-find-helm)
  (defun mymy-calibredb-update-list ()
    (interactive)
    (setq calibredb-search-entries (calibredb-candidates))
    (setq calibredb-full-entries calibredb-search-entries)
    )
  (setq calibredb-root-dir "~/Sync/CalibreLibrary/")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(("~/Sync/")))
  (setq calibredb-helm-actions
        (helm-make-actions
         "Open file"                   'calibredb-find-file
         "View details"                'calibredb-show-entry
         "Open file other frame"       'calibredb-find-file-other-frame
         "Open file with default tool" (lambda (candidate)
                                         (calibredb-open-file-with-default-tool nil candidate))
         "Open Cover Page"             'calibredb-find-cover
         "Set tags"          'calibredb-set-metadata--tags
         "Set comments"      'calibredb-set-metadata--comments
         "List fileds" 'calibredb-set-metadata--list-fields
         "Show metadata"               'calibredb-show-metadata
         "Export"                      'calibredb-export
         "Remove"                      'calibredb-remove
         "Insert an org link"          (lambda (candidate)
                                         (unless (featurep 'org)
                                           (require 'org))
                                         (if (fboundp 'org-insert-link)
                                             (kill-new (format "[[%s][]]" (calibredb-getattr candidate :file-path)))))
         "Mail Add attachment"         (lambda (candidate)
                                         (mail-add-attachment (calibredb-getattr candidate :file-path))))))

;;** Spreedsheet
;; Will keep this two just in case
(use-package rainbow-numbers-mode
  :straight nil
  :load-path "~/.emacs.d/elisp/rainbow-numbers-mode.el")
(use-package ses-mode
  :no-require
  :straight nil
  :config
  (setq ses-after-entry-functions '(next-line))
  :hook
  (ses-mode . rainbow-numbers-mode))
;;** Lisp things
(use-package lispy
  :defer t
  :init
  (setq lispy-compat '(edebug cider))
  (el-patch-defun lispy-undo ()
    "Deactivate region and `undo'."
    (interactive)
    (when (region-active-p)
      (deactivate-mark t))
    (el-patch-swap (undo) (undo-tree-undo)))
  :hook
  (clojure-mode . lispy-mode)
  (emacs-lisp-mode . lispy-mode)
  (lisp-mode . lispy-mode)
  )

(use-package clj-refactor
  :defer
  :init
  (cljr-add-keybindings-with-prefix "C-c m")
  :hook
  (cider-mode . clj-refactor-mode))

(use-package flycheck-clj-kondo)

(use-package cider
  :defer
  :custom
  (cider-test-show-report-on-success t)
  :hook
  (clojure-mode . cider-mode)
  (cider-repl-mode . aggressive-indent-mode)
  (cider-repl-mode . subword-mode)
  (cider-repl-mode . cider-company-enable-fuzzy-completion)
  (cider-mode . cider-company-enable-fuzzy-completion))

(use-package clojure-mode
  :config (require 'flycheck-clj-kondo))
;;** lsp
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setenv "LSP_USE_PLISTS" "true")
  (setq lsp-use-plists t)
  :config
  ;; For debugging
  ;; (setq lsp-log-io t)
  (setq lsp-log-io nil)
  ;; Please forgive my soul for being foolish
  ;; (setq lsp-keep-workspace-alive t)
  :hook
  ((lsp-mode . lsp-enable-which-key-integration)))

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

(use-package lsp-pyright
  :hook
  ;; If i were to delete this section, it will not make my init fail
  ((python-mode . lsp)
   (python-mode . (lambda () (require 'lsp-pyright)))))

(use-package lsp-haskell
  ;; Uncontable tales i have of how this monster have ruined my day, not
  ;; because of itself, but because Of how much ram it needs and how my
  ;; little School-gorverment-given computer hogs from the effort of
  ;; keeping this thing afloat
  ;; :hook
  ;; (haskell-mode . lsp)
  ;; (haskell-literate-mode . lsp)
  )
;;** Python
(use-package python
  :bind (:map python-mode-map
              (("C-c C-q" . jupyter-eval-buffer)
               ("C-c C-j" . jupyter-run-repl)))
  :config
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--colors=Linux --profile=default --simple-prompt -i"
        python-shell-prompt-regexup "In \\[[0-9]+\\]: "
        python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
        python-shell-completion-setup-code
        "from IPython.core.completerlib import module_completion"
        python-shell-completion-module-string-code
        "';'.join(module_completion('''%s'''))\n"
        python-shell-completion-string-code
        "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
  (dolist (mode-iter '(python-mode))
    (font-lock-add-keywords mode-iter
                            '(("\\([@~^&\|!<>:=\\+*/%-]\\)" 0
                               'font-lock-operator-face keep)
                              ("\\<[\\+-]?[0-9]+\\(.[0-9]+\\)?\\>" 0
                               'font-lock-number-face keep)
                              )))
  :hook
  ((inferior-python-mode . hide-mode-line-mode)
   (python-mode . (lambda ()
                    (setq-local fill-column 79
                                company-dabbrev-char-regexp "\\sw\\|\\s_")))
   (python-mode . (highlight-indentation-mode))
   (python-mode . (electric-operator-mode))
   (python-mode . (hs-minor-mode))))

(use-package pydoc
  :config
  ;; Better to keep it self-contained
  (general-define-key
   :keymaps 'python-mode-map
   "C-c C-d C-d" (if (string-equal "0" (s-trim-right (shell-command-to-string "python -c \"import jedi\" && echo $?")))
                     #'pydoc-at-point #'pydoc-at-point-no-jedi)
   "C-c C-d C-v" 'pydoc))

(use-package blacken
  :hook (python-mode . blacken-mode)
  :config (setq blacken-line-length 79))

(use-package poetry
  :config
  (setenv "WORKON_HOME" "~/.cache/pypoetry/virtualenvs/")
  :hook
  (python-mode . poetry-tracking-mode))
;;** Haskell
(use-package haskell-mode)
;;** Company
(use-package company
  :config
  ;; Prefer to use it manually
  (setq company-idle-delay nil)
  ;; Why would i?, dabbrev suggestions is case sensitive
  (setq company-dabbrev-downcase nil)
  ;; Actually it doesn't, welp my mistake
  (setq company-dabbrev-ignore-case nil)
  (setq company-minimum-prefix-length 3)
  (setq company-selection-wrap-around t)
  (setq company-require-match nil)
  (setq company-quick-access-keys '("n" "e" "i" "o" "u" "y" "m" "k" "h" "l"))
  ;; M-<n> where is a element of company-quick-access-keys
  (setq company-show-quick-access t)
  (setq company-tooltip-align-annotations t)
  (setq company-frontends
        '(company-pseudo-tooltip-unless-just-one-frontend
          company-preview-if-just-one-frontend
          company-echo-metadata-frontend
          company-pseudo-tooltip-frontend))
  (general-define-key
   :keymaps 'company-active-map
   "TAB" 'company-select-next
   "<backtab>" 'company-select-previous
   ;; "ESC" 'company-abort
   ;; "C-SPC" 'company-select-next-or-abort
   ;; "C-S-SPC" 'company-select-previous-or-abort
   )
  (company-tng-mode)
  (global-company-mode))

(use-package company-quickhelp
  :config
  (general-define-key
   :keymaps 'company-active-map
   "C-c h" 'company-quickhelp-manual-begin)
  (company-quickhelp-mode))

;;** flycheck
(use-package flycheck
  :bind ( :map flycheck-mode-map
          ("M-n" . flycheck-next-error)
          ("M-u" . flycheck-previous-error)))
;;** Helm
(use-package helm
  :straight t
  :straight helm-swoop helm-projectile
  :diminish helm-mode
  :init
  (setq helm-split-window-default-side 'rigth)
  (setq helm-bookmark-show-location t)
  (setq helm-buffers-fuzzy-matching t)
  ;; TODO: Move this to org mode
  (setq org-cycle-include-plain-lists 'integrate)
  (setq helm-mini-default-sources
        '(helm-source-buffers-list
          helm-source-recentf
          helm-source-buffer-not-found))
  (spaceline-helm-mode t)
  (spaceline-toggle-helm-number-on)
  (helm-mode)
  :config
  ;; Randomly found in
  ;; https://emacs.stackexchange.com/questions/15051/can-helm-apropos-display-the-key-bindings-for-commands-the-way-helm-m-x-does
  (el-patch-defun helm-def-source--emacs-commands (&optional default)
    (helm-build-in-buffer-source "Commands"
      :init (lambda ()
              (helm-apropos-init 'commandp default))
      :fuzzy-match helm-apropos-fuzzy-match
      :filtered-candidate-transformer (and (null helm-apropos-fuzzy-match)
                                           'helm-apropos-default-sort-fn)
      :display-to-real 'helm-symbolify
      (el-patch-add :candidate-transformer 'helm-M-x-transformer-1)
      :nomark t
      :persistent-action (lambda (candidate)
                           (helm-elisp--persistent-help
                            candidate 'helm-describe-function))
      :persistent-help "Toggle describe command"
      :action 'helm-type-function-actions))
  :config
  (general-define-key
   "M-x" 'helm-M-x
   "C-x b" 'helm-mini
   "C-x C-f" 'helm-find-files
   "C-x r b" 'helm-filtered-bookmarks
   "C-c h s" 'helm-swoop
   "C-c t h" 'helm-projectile)
  (general-define-key
   :keymaps 'helm-map
   "C-n" 'helm-next-line
   "C-u" 'helm-previous-line)
  :hook
  (helm-minibuffer-set-up . helm-exchange-minibuffer-and-header-line))

(use-package helm-lsp
  :after helm)

;;** ispell
(use-package ispell
  ;; https://200ok.ch/posts/2020-08-22_setting_up_spell_checking_with_multiple_dictionaries.html
  :config
  (setq ispell-program-name "hunspell")
  ;; Configure German, Swiss German, and two variants of English.
  (setq ispell-dictionary "en_US,es_ES")
  (setq ispell-alternate-dictionary (expand-file-name (concat dropbox-dir "english_list.txt")))
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,es_ES")
  ;; For saving words to the personal dictionary, don't infer it from
  ;; the locale, otherwise it would save to ~/.hunspell_de_DE.
  (setq ispell-personal-dictionary (concat dropbox-dir ".hunspell_personal"))
  ;; The personal dictionary file has to exist, otherwise hunspell will
  ;; silently not use it.
  (unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0)))
;;** Magit
(use-package magit)

;;** Dired
(use-package dired+)
(use-package dired-subtree
  :bind (:map dired-mode-map
              ("i" . dired-subtree-toggle)))
(use-package dired-collapse
  :hook
  ((dired-mode . dired-collapse-mode)))

;; TODO: Put this in org roam section
(use-package git-auto-commit-mode)
;;** Ledger mode
(use-package ledger-mode)
;;** Music
(use-package emms
  ;; Overkill, but extremely nice
  :disabled
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  (setq emms-source-file-default-directory "~/Music/")
  )
(use-package empv
  :straight (empv :type git :host github :repo "isamert/empv.el")
  :config
  (setq empv-base-directory (expand-file-name "~"))
  (defhydra hydra-empv-volume
    ()
    ("-" empv-volume-down)
    ("s" empv-volume-set)
    ("+" empv-volume-up)
    ("." nil :color blue)
    )
  :bind
  ("C-c e n" . empv-playlist-next)
  ("C-c e p" . empv-playlist-prev)
  ("C-c e s" . empv-playlist-shuffle)
  ("C-c e e" . empv-toggle)
  ("C-c e l" . empv-playlist-loop-on)
  ("C-c e f" . empv-play-file)
  ("C-c e v" . hydra-empv-volume/body)
  )

;;** Org mode
(use-package org
  :straight t
  ;; :straight helm-org
  :ryo
  (:mode 'org-agenda-mode)
  ("okc" org-agenda-exit)
  :init
  (setq org-clock-string-limit 25)
  (setq spaceline-org-clock-format-function 'dwim/org-clock-get-string)
  (require 'org-habit)
  (require 'ob-clojure)
  (setq org-babel-clojure-backend 'cider)
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
     (sqlite . t)
     (clojure . t)
     (java . t)
     ))
  :config
  ;; For org roam
  (require 'org-protocol)
  (define-key org-mode-map (kbd "C-j") 'nil)
  ;; Unneeded
  ;; (define-key org-mode-map (kbd "C-j") (lambda (count)
  ;;                                        (interactive "p")
  ;;                                        ))
  ;; (add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
  ;; (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags))
  (defun mymy-refile-to-done ()
    (interactive)
    (my/refile (concat org-roam-directory "2021-12-05-08-48-44-done.org") "Done"))
  (org-id-find 'add2b103-aa51-4d04-a741-7c2136bbca69)
  (advice-add 'org-clock-get-clocked-time :around (lambda (f) (if (org-clocking-p) (funcall f) 0)))
  ;; (add-hook 'org-capture-mode-hook #'(lambda () (make-frame) (delete-window)))
  (defun mymy-org-mode-agenda ()
    (interactive)
    (let ((org-agenda-window-setup 'only-window))
      (org-agenda nil "n")
      (setq-local mode-line-format nil)
      (centaur-tabs-local-mode)))
  ;;; org element
  (defun mymy-org-element-parse-string (s &optional parser)
    (with-temp-buffer
      (let ((org-inhibit-startup nil))
        (insert s)
        (org-mode)
        (goto-char (point-min))
        (funcall (or parser #'org-element-parse-buffer)))))
  (defun mymy-org-element-parse-link (link)
    (mymy-org-element-parse-string link #'org-element-link-parser))
  (setq org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS %6CLOCKSUM(Clock) %8Effort(Effort)")
  ;; Press C-c to deactivate this temporarily
  (setq org-fast-tag-selection-single-key t)
  :custom
  (org-latex-pdf-process
   ;; -pdfxe: use xelatex -pdflua: use luatex -bibtex use bibtex when needed
   ;; -xelatex use xelatex for processing files to pdf and turn dvi/ps modes off
   ;; -f: Force -pdf output pdf -bibtex
   ;; (list "latexmk -bibtex -f -pdf %f")
   (list "latexmk -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f"))
  ;; ▶, ▼, ↴, ⬎, ⤷, and ⋱
  (org-ellipsis "▶")
  (org-log-done t)
  ;; (org-hide-emphasis-markers t)
  (org-refile-targets '((nil :maxlevel . 2)))
  (org-catch-invisible-edits 'error)
  (org-special-ctrl-a/e t)
  (org-habit-show-all-today t)
  (org-return-follows-link nil)
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
  (org-startup-folded t)
  (org-default-notes-file (concat main-dropbox-dir "agenda.org"))
  (org-todo-keyword-faces
   '(("CANCELLED" . (:foreground "red" :weight bold))
     ("ABANDONED" . (:foreground "red" :weight bold))
     ("CLASS" . (:foreground "purple" :weight bold))
     ("NEXT" . (:foreground "blue" :weight bold))
     ("HABIT" . (:foreground "yellow" :weight italic))
     ;; ("PROJECT" . (:foreground "white" :weight bold))
     ;; ("COMPLETED" . (:foreground "cyan" :weight bold))
     ))
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(s)")
     (type "HABIT(h)" "|" "CANCELLED(c)" "ABANDONED(a)")))
  ;; (org-capture-templates
  ;;  ;; https://orgmode.org/manual/Template-expansion.html#Template-expansion
  ;;  ;; https://orgmode.org/manual/Template-elements.html#Template-elements
  ;;  '(
  ;;    )
  ;;  )
  (org-enforce-todo-dependencies t)
  (org-tag-faces
   '(("Hold" (:foreground "yellow" :weight bold))
     ("Kobo" (:foreground "red" :weight bold))))
  (org-format-latex-options '(plist-put org-format-latex-options :scale 2.0 :background auto :foreground "white"))
  (org-highlight-latex-and-related '(latex script entities))
  (org-image-actual-width nil)
  (org-log-into-drawer t)
  :bind (("C-c o c" . org-capture)
         :map org-mode-map
         (("C-M-k" . company-files))
         )
  :hook
  (org-mode . org-superstar-mode)
  (org-mode . prettify-symbols-mode)
  (org-mode . org-indent-mode)
  (org-mode . flyspell-mode)
  (org-mode . auto-fill-mode)
  (org-mode . org-super-agenda-mode)
  (kill-emacs . (lambda () (org-clock-out nil t)))
  (org-mode . (lambda () (setq-local tab-width 2
                                     indent-tabs-mode nil
                                     python-shell-interpreter "python3"))))

;;*** Org latex
(use-package org-fragtog
  ;; For another time
  :disabled
  :hook
  (org-mode . org-fragtog-mode))

(use-package org-latex-impatient
  :defer t
  :hook (org-mode . org-latex-impatient-mode)
  :init
  (setq org-latex-impatient-tex2svg-bin
        ;; location of tex2svg executable
        "~/node_modules/mathjax-node-cli/bin/tex2svg")
  ;; (setq org-latex-impatient-posframe-position-handler #'posframe-poshandler-frame-top-right-corner)
  (setq org-latex-impatient-posframe-position-handler #'org-latex-impatient-poshandler)
  :bind
  (("C-c i i" . org-latex-impatient-start)
   ("C-c i I" . org-latex-impatient-stop)))
;;*** org super agenda
(use-package org-super-agenda
  :bind (:map org-super-agenda-header-map
              ("n" . org-agenda-next-line)
              ("u" . org-agenda-previous-line)
              :map org-agenda-mode-map
              ("n" . org-agenda-next-line)
              ("u" . org-agenda-previous-line))
  :config
  (ryo-modal-major-mode-keys
   'org-agenda-mode
   ("n" org-agenda-next-line)
   ("u" org-agenda-previous-line)
   ("oks" org-agenda-write)
   ("rl" org-toggle-link-display)
   ("ro" org-agenda-open-link))
  ;; Previously called org-agenda-ndays
  (setq org-agenda-span 1)
  ;; (setq org-agenda-start-day "-1d")
  (setq org-agenda-start-day "1d")
  (setq org-super-agenda-groups
        '((:name "Doing" :todo "NEXT")
          (:name "Projects" :children todo)
          (:auto-priority)
          (:discard (:todo "DONE"))
          (:name "Stuck" :anything)
          ;; (:discard (:anything t))
          )))
;;; I STOPPED HERE
(use-package posframe)

(use-package which-key-posframe
  :config
  (which-key-posframe-mode))
(use-package company-posframe
  :disabled
  :config
  (company-posframe-mode))
(use-package helm-posframe
  :disabled ;; To damn slow
  :config
  (helm-posframe-enable)
  ;; (helm-posframe-disable)
  ;; https://www.reddit.com/r/emacs/comments/ovr6ti/preserve_minibuffer_focus/h7bavau/
  (defun switch-to-minibuffer-window (&rest args)
    "switch to minibuffer window (if active)"
    (when (active-minibuffer-window)
      (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
      (select-window (active-minibuffer-window))))
  (defun preserve-minibuffer-focus ()
    "keep the minibuffer in focus"
    (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
      (switch-to-minibuffer-window)))
  )
(use-package org-journal
  ;; Dailies is a better alternative
  :disabled
  ;; Why do i live, just to suffer?
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j")
  :config
  (setq org-journal-dir (concat main-dropbox-dir "journal/")
        org-journal-date-format "%A, %d %B %Y")
  )
(use-package org-superstar
  :config
  (org-superstar-configure-like-org-bullets)
  (setq org-superstar-headline-bullets-list '(?▹ ?⭆ ?○ ?✸ ?✿ ?✥ ?❂ ?❄))
  )
(use-package elisp-slime-nav
  :config
  ;; elisp-slime-nav-describe-elisp-thing-at-point C-c C-d C-d
  (if (s-suffix\? "laptop" (system-name))
      (bind-key "C-." 'elisp-slime-nav-find-elisp-thing-at-point)
    (bind-key "M-." 'elisp-slime-nav-find-elisp-thing-at-point)
    )
  :hook
  (emacs-lisp-mode . elisp-slime-nav-mode)
  )
(use-package csv-mode)
(use-package yasnippet-snippets)
(use-package org-roam-ui
  ;; :disabled
  :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :straight websocket simple-httpd
  :after org-roam
  :config
  (require 'org-roam-protocol)
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))
(use-package git-timemachine
  :straight t
  :ryo
  ("Gt" git-timemachine-toggle)
  ("GT" hydra-git-timemachine/body)
  :bind (:map git-timemachine-mode-map
              ("n" . git-timemachine-show-next-revision)
              ("u" . git-timemachine-show-previous-revision))
  :hook
  ((git-timemachine-mode . hydra-git-timemachine/body))
  (defhydra hydra-git-timemachine (:hint nil)
    "
| _n_ Next _w_ Copy abbrev hash _g_ Goto nth    _b_ Blame _q_uit
|==============================================================|[_._]: quit
| _u_ Prev _W_ Copy full hash   _t_ Goto by msg _c_ Show Magit"
    ("n" git-timemachine-show-next-revision)
    ("u" git-timemachine-show-previous-revision)
    ("w" git-timemachine-kill-abbreviated-revision)
    ("W" git-timemachine-kill-revision)
    ("g" git-timemachine-show-nth-revision)
    ("t" git-timemachine-show-revision-fuzzy)
    ("q" git-timemachine-quit)
    ("b" git-timemachine-blame)
    ("c" git-timemachine-show-commit)
    ("? " git-timemachine-help)
    ("." nil :color blue)))
(use-package perspective
  :disabled
  (defhydra hydra-persp (:hint nil)
    "
|_n_ Next _a_ Add buffer _r_ rename
|==================================|[_q_] Quit
|_u_ Prev _A_ Set buffer _k_ kill persp
"
    ("n" persp-next)
    ("u" persp-prev)
    ("a" persp-add-buffer)
    ("A" persp-set-buffer)
    ("k" persp-kill)
    ("r" persp-rename)
    ("q" nil :color blue)
    )
  :ryo
  ("S"
   (("y"
     (("s" persp-switch)
      ("k" persp-kill :name "Kill perspective")
      ("r" persp-rename)
      ("i" persp-import)
      ("S" persp-state-save)
      ("L" persp-state-load)
      ("m" hydra-persp/body :name "hydra-persp")
      ))))
  :config
  (setq persp-state-default-file (concat user-emacs-directory "persp")
        )
  :hook
  (after-init . persp-mode)
  (kill-emacs . persp-state-save)
  )
(use-package vulpea :after org-roam
  :disabled
  :config
  (defun my-vulpea-insert-handle (note)
    "Hook to be called on NOTE after `vulpea-insert'."
    (when-let* ((title (vulpea-note-title note))
                (tags (vulpea-note-tags note)))
      (when (seq-contains-p tags "Ref")
        (org-roam-tag-add '("literature_node")))))
  (add-hook 'vulpea-insert-handle-functions
            #'my-vulpea-insert-handle)
  )

(use-package helm-bibtex
  :config
  (defvar helm-source-bibtex
    (helm-build-sync-source "BibTeX entries"
      :header-name (lambda (name)
                     (format "%s%s: " name (if helm-bibtex-local-bib " (local)" "")))
      :candidates 'helm-bibtex-candidates
      :filtered-candidate-transformer 'helm-bibtex-candidates-formatter
      :action (helm-make-actions
               ;; Moved "Edit notes" to the default action
               "Edit notes"                 'helm-bibtex-edit-notes
               "Open PDF, URL or DOI"       'helm-bibtex-open-any
               "Open URL or DOI in browser" 'helm-bibtex-open-url-or-doi
               "Insert citation"            'helm-bibtex-insert-citation
               "Insert reference"           'helm-bibtex-insert-reference
               "Insert BibTeX key"          'helm-bibtex-insert-key
               "Insert BibTeX entry"        'helm-bibtex-insert-bibtex
               "Attach PDF to email"        'helm-bibtex-add-PDF-attachment
               "Show entry"                 'helm-bibtex-show-entry
               "Add PDF to library"         'helm-bibtex-add-pdf-to-library))
    "Source for searching in BibTeX files.")
  )
(use-package bibtex
  :config
  (setq bibtex-autokey-year-length 4))
(use-package org-ref
  :config
  ;; (setq bibtex-completion-bibliography (list (concat main-dropbox-dir "Main.bib")))
  (setq bibtex-completion-bibliography '("~/tmp/ZoteroToEbib/MyLibrary/MyLibrary.bib"))
  (setq bibtex-completion-pdf-field "file"))

(use-package ebib
  :config
  (el-patch-defun ebib-create-org-identifier (key _)
    "Create a unique identifier for KEY for use in an org file.
The key is prepended with the string \"Custom_id:\", so that it
can be used in a :PROPERTIES: block."
    (format (el-patch-swap ":Custom_id: %s" ":ROAM_REFS: [[cite:&%s]]") key))
  (el-patch-defun ebib-create-org-description (key db)
    "Return a description for an Org mode note for KEY in DB.
The title is formed from the author(s) or editor(s) of the entry,
its year and its title.  Newlines are removed from the resulting
string."
    (let ((author (or (ebib-get-field-value "author" key db 'noerror 'unbraced 'xref)
                      (ebib-get-field-value "editor" key db 'noerror 'unbraced 'xref)
                      "(No Author)"))
          (el-patch-swap (year (ebib-get-year-for-display key db))
                         (date (ebib-get-field-value "date" key db 'noerror 'unbraced 'xref)))
          (title (or (ebib-get-field-value "title" key db 'noerror 'unbraced 'xref)
                     "(No Title)")))
      (el-patch-splice 2 0 ;; Remove two from the front 0 from the rear
        (remove ?\n (format (el-patch-swap "%s (%s): %s" "Title: %s\nDate: %s\nAuthor: %s")
                            (el-patch-swap author title) (el-patch-swap year date) (el-patch-swap title author))))))
  :config
  (setq ebib-preload-bib-files '("~/tmp/ZoteroToEbib/MyLibrary/MyLibrary.bib"))
  (setq ebib-bibtex-dialect 'biblatex)
  (setq ebib-file-associations '(("pdf" . "sioyek")
                                 ("ps" . "zathura")))
  (setq ebib-default-directory "~/tmp/ZoteroToEbib/MyLibrary/")
  (setq ebib-popup-entry-window t)
  (setq ebib-layout 'index-only)
  (setq ebib-reading-list-file (concat main-dropbox-dir "ReadingList.org"))

  ;; Ebib doesn't need to manage this
  (setq ebib-notes-directory org-roam-directory)
  (setq ebib-notes-storage 'multiple-notes-per-file)
  (setq ebib-notes-default-file (concat org-roam-directory "2022-01-09-12-38-23-zettelkasten.org"))
  ;; (setq ebib-notes-use-org-capture nil)
  (setq ebib-notes-use-org-capture "r")
  ;; Double %% for org expansions
  (setq ebib-notes-template "* lit %%<%%s>%%?\n:PROPERTIES:\n:ID:         %%(org-id-new)\n%K\n:END:\n%T\n\n\n")
  (add-to-list 'org-capture-templates
               `("r" "bibliopraphic reference" entry
                 (file+olp ,ebib-notes-default-file "Zettelkästen" "lit notes")
                 (function ebib-notes-create-org-template)))
  :bind (;; ("C-c n" . ebib)
         :map ebib-index-mode-map
         ("u" . ebib-prev-entry)
         ("p" . ebib-browse-url)
         :map ebib-entry-mode-map
         ("u" . ebib-prev-entry)
         ("p" . ebib-browse-url)
         ))
(use-package ebib-biblio
  :disabled
  :after (ebib biblio)
  :bind (:map biblio-selection-mode-map
              ("e" . ebib-biblio-selection-import)))
;; Looks good
;; Even though this package already have some utilities that i have
;; implemented there are some that are far better done than mine. So
;; with only a little work i can steal some code or make some glue
;; with how i want some things to work
(use-package delve
  ;; For some reason it just break, maybe i will report this
  ;; Found out that the reason is because show/hide outline
  ;; functionality is buggy with lister
  :straight lister
  :straight (delve :host github :type git :repo "publicimageltd/delve")
  ;; :init
  ;; (setq delve-minor-mode-prefix-ey (kbd "C-c d"))
  :config
  ;; Notes on one problem of delve, whenever i press RET after TAB the
  ;; whole line dispapears except of the ellipsis
  ;; buffer-invisibility-spec
  ;; It't what is in charge of putting ellipis whenever a text has the
  ;; invisible property
  ;; At the end the problem wasn't cause because of this, but because
  ;; of something inside of the sublist of lister
  ;; Maybe the problem is with the refresher?
  ;; (lister-refresh-at lister-local-ewoc :point)
  ;; The problem is ewoc-invalidate, i think is because it should also include the sublists
  ;; (el-patch-defun lister-refresh-at (ewoc pos-or-node)
  ;;   "In EWOC, redisplay the node at POS-OR-NODE."
  ;;   (el-patch-wrap 1 0
  ;;     (save-excursion
  ;;       (lister-with-node ewoc pos-or-node node
  ;;         ((el-patch-swap ewoc-invalidate ewoc-refresh) ewoc)))))
  ;; Tested and works well, execpt that it messes up location after refresh
  ;; TODO: Try to add the sublist in lister--parse-position
  ;; Prototype
  ;; (if (lister-sublist-below-p lister-local-ewoc :point)
  ;;     (progn
  ;;       (vconcat (lister--parse-position lister-local-ewoc :point)
  ;;                (lister--parse-position lister-local-ewoc :next)))
  ;;   (lister--parse-position lister-local-ewoc :point))
  ;; Found a better solution
  ;; (let ((ewoc lister-local-ewoc))
  ;;   (if (lister-with-sublist-below ewoc :point beg end
  ;;         (lister--outline-invisible-p ewoc beg))
  ;;       (progn
  ;;         (lister-mode-cycle-sublist ewoc :point)
  ;;         (lister-refresh-at ewoc :point)
  ;;         (lister-mode-cycle-sublist ewoc :point))
  ;;     (lister-refresh-at ewoc :point)))
  (el-patch-defun delve--key--toggle-preview (zettel &optional prefix)
    "Toggle the display of the preview of ZETTEL.
  With PREFIX, open ZETTEL's file in a buffer."
    (interactive (list (delve--current-item-or-error 'delve--zettel) current-prefix-arg))
    (if prefix
        (delve--key--open-zettel zettel)
      (let ((preview (and (not (delve--zettel-preview zettel))
                          (or (delve--get-preview-contents zettel)
                              "No preview available"))))
        (setf (delve--zettel-preview zettel) preview)
        (el-patch-swap
          (lister-refresh-at ewoc :point)
          (let ((ewoc lister-local-ewoc))
            (if (lister-with-sublist-below ewoc :point beg end
                  (lister--outline-invisible-p ewoc beg))
                (progn
                  (lister-mode-cycle-sublist ewoc :point)
                  (lister-refresh-at ewoc :point)
                  (lister-mode-cycle-sublist ewoc :point))
              (lister-refresh-at ewoc :point)))))))

  (el-patch-defun delve--key--collect-into-buffer (ewoc &optional move)
    "In Delve EWOC, copy all marked items into a (new) collection.
If region is active, also collect the items in that region.  If
MOVE is non-nil, also delete the items in the source buffer,
effectively moving the marked items.  Switch to the target
collection."
    (interactive (list lister-local-ewoc current-prefix-arg))
    (delve--maybe-mark-region ewoc)
    (unless (lister-items-marked-p ewoc)
      (el-patch-swap
        (user-error "No items marked")
        (lister-mark-unmark-at ewoc :point t)
        ))
    (let (acc)
      (lister-walk-marked-nodes ewoc
                                (lambda (_ node)
                                  (push (lister-node-get-data node) acc)))
      (let* ((counted-items (format "%d items" (length acc)))
             (prompt        (format "%s %s to collection: "
                                    (if move "Move" "Add")
                                    counted-items))
             (target      (delve--add-to-buffer (nreverse acc) prompt)))
        ;; now we can safely delete the source nodes
        (when move
          (lister-delete-marked-list ewoc))
        (switch-to-buffer target)
        (message (format "%s %s to this buffer"
                         (if move "Moved" "Added")
                         counted-items)))))

  (el-patch-defun delve--remove-candidate-suffix (cand)
    "Remove suffix in parentheses from CAND."
    (replace-regexp-in-string (el-patch-swap
                                (rx string-start
                                    (zero-or-more space)
                                    (group (*? nonl))
                                    (zero-or-more space)
                                    "(" (one-or-more nonl) ")")
                                (rx (one-or-more space)
                                    "(" (+ (not ")")) ")"
                                    string-end))
                              "\\1"
                              cand))

  (defun mymy-delve-parse-link (link)
    "Return the name, type and path of org link as plist. The string
    just needs to have a link and the link used is the first one"
    (if (save-match-data
          ;; Check that it's a org link and set the first occurence as
          ;; the link
          (and (string-match org-bracket-link-regexp link)
               (setq link (match-string 0 link))))
        (let* ((get (lambda (n) (replace-regexp-in-string org-bracket-link-regexp n link)))
               (name (funcall get "\\2"))
               (link-props (split-string (funcall get "\\1") ":")))
          `(:type ,(car link-props) :path ,(cadr link-props) :name ,name))))

  :config
  (setq delve-dashboard-tags '("entry"))
  (setq delve-display-path nil)
  (setq delve-storage-paths (concat main-dropbox-dir "Notes-Delve/"))
  (abbreviate-file-name delve-storage-paths)
  (setq delve--no-icons nil)
  (add-hook #'delve-mode-hook #'delve-compact-view-mode)
  ;; Makes it so that when you kill, kill the node at point
  ;; This is set local to the buffer because i like the actual behaivour of kill-line
  (add-hook #'delve-mode-hook (lambda () (setq-local kill-whole-line t)))
  (delve-global-minor-mode)
  (ryo-modal-keys
   ("Sd"
    (("c" delve-minor-mode-collect-actions)
     ("i" delve-minor-mode-inspect-actions)
     ("d" delve))))
  ;; Delve doen't know how to deal when `delve--last-selected-buffer' is nil
  ;; The problems is that it cannot be nil bc the function propertize
  ;; doesn't accept a nil
  (ryo-modal-major-mode-keys
   'delve-mode
   ("n" "C-n")
   ("u" "C-p")
   ("N" "C-r")
   ("U" "C-s")
   ("h" lister-mode-backward-same-level)
   ("l" lister-mode-forward-same-level)
   ("M-n" lister-mode-down)
   ("M-u" lister-mode-up)
   ("M-N" lister-mode-left)
   ("M-U" lister-mode-right)
   ("d" delve--key--multi-delete))
  ;; This two can wait
  ;; TODO: Make a function that copies the name of the current node
  ;; TODO: Make a function that insert a description list as a node with a description
  ;; DONE: Make a function that inserts the current copied link as a delve object
  ;; delve--key--yank
  ;; delve--yank-handler
  ;; Need to create a type for a id link
  ;; (cl-defstruct (delve--zettel
  ;;                (:include delve--item)
  ;;                (:constructor delve--zettel-create (node)))
  ;;   "A Zettel item storing an Org Roam node."
  ;;   node preview out-of-sync info)
  ;; TODO: Find more about delve--note
  )

;;** Org roam
(use-package org-roam
  :init
  (defvar org-roam-v2-ack t)
  (defun phc-prognify-and-append (form new-form)
    "Wrap FORM in `)progn' if needed and append NEW-FORM"
    (cond ((and (consp form)
                (eq (car form) 'progn))
           (append form (list new-form)))
          ((null form)
           `(progn ,new-form))
          (t
           `(progn ,form ,new-form))))
  (set-default 'native-comp-async-env-modifier-form
                (phc-prognify-and-append
                 (when (boundp 'native-comp-async-env-modifier-form)
                   native-comp-async-env-modifier-form)
                 `(defvar org-roam-v2-ack ,org-roam-v2-ack)))
  (setq mymy-org-roam-visit-node-other-window t)
  :config
  (setq org-agenda-files (list (concat mymy-org-roam-dir "Projects/20210715113548-projects.org")))
  (require 'org-roam-patch)
  (org-roam-db-autosync-enable)
  ;; https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/

  (defun mymy-org-roam-in-zettelaksten (node)
    (string-equal (expand-file-name (concat mymy-org-roam-dir "2022-01-09-12-38-23-zettelkasten.org"))
                  (org-roam-node-file node)))

  ;; TODO: Find a way that this works
  ;; (let ((node (org-roam-node-read)))
  ;;   (if (org-roam-node-file node)
  ;;       (let ((mymy-org-roam-visit-node-other-window nil))
  ;;         (org-roam-node-visit node))
  ;;     (org-roam-capture-
  ;;      :node node
  ;;      :templates templates
  ;;      :props '(:finalize find-file))))

  ;; Make use only of notes that are in the zettelaksten
  (el-patch-defun org-roam-node-random (&optional other-window filter-fn)
    "Find and open a random Org-roam node.
With prefix argument OTHER-WINDOW, visit the node in another
window instead.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out."
    (interactive current-prefix-arg)
    (org-roam-node-visit
     (cdr (seq-random-elt (org-roam-node-read--completions #'mymy-org-roam-in-zettelaksten)))
     other-window))

  (defun mymy-org-roam-ref-find (&optional other-window)
    (interactive)
    (org-roam-node-find
     other-window
     nil
     (my/org-roam-filter-by-tag "Ref")))
  ;; https://d12frosted.io/posts/2020-07-07-task-management-with-roam-vol4.html#:~:text=(defun%20org%2Droam,node%2Dinsert%2Dwrapper)
  (defun mymy-org-roam-node-insert-wrapper (node)
    "Insert a link to the note using FN.

  If inserted node has REF tag on it, tag the current file
  as a literature node"
    (interactive)
    (when (and (seq-contains-p (org-roam-node-tags node) "Ref")
               (not (seq-contains-p (ignore-errors (org-roam-node-tags (org-roam-node-at-point))) "Ref")))
      (org-roam-tag-add '("literature_node"))))

  ;; (advice-add #'org-roam-node-insert :around #'mymy-org-roam-node-insert-wrapper)
  ;; (advice-remove #'org-roam-node-insert #'mymy-org-roam-node-insert-wrapper)
  (add-hook 'mymy-org-roam-node-insert-hook #'mymy-org-roam-node-insert-wrapper)

  ;; Not using second dailies, consider removing the feature
  ;; Second dailies
  (setq org-roam-dailies-second-directory "dailies/")
  (defun mymy-org-roam-dailies-update-second-capture-templates (&rest args)
    (setq org-roam-dailies-second-capture-templates
          `(("d" "date" entry
             "* %<%H:%M> %?"
             :target (file+head+olp "%<%Y-%m-%d>.org"
                                    "#+title: %<%Y-%m-%d>\n#+STARTUP: nofold\n"
                                    (,(format-time-string "%A, %d %B %Y")))
             :unnarrowed t))))
  (mymy-org-roam-dailies-update-second-capture-templates)
  ;; Update after open capture
  (add-hook 'org-capture-mode-hook #'mymy-org-roam-dailies-update-second-capture-templates)

  (defun mymy-org-roam-dailies--secondary-capture (time &optional goto)
    (let ((org-roam-dailies-directory
           org-roam-dailies-second-directory)
          (org-roam-dailies-capture-templates
           org-roam-dailies-second-capture-templates))
      (org-roam-dailies--capture time goto)))
  (defun mymy-org-roam-dailies-secondary-capture-today (&optional goto)
    (interactive "P")
    (mymy-org-roam-dailies--secondary-capture (current-time) goto))

  (defun mymy-org-roam-dailies-secondary-goto-today ()
    (interactive)
    (mymy-org-roam-dailies-secondary-capture-today t))
  ;; (require 'org-roam-protocol)

  ;; If a note is a heading, in what file is it?
  ;; NOTE: I found that aliases are nodes that share the same information
  ;; As the original node except the title.
  (cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
    (concat (when (> (org-roam-node-level node) 0)
              (concat "> " (propertize
                            (org-roam-node-file-title node)
                            'face 'mymy-org-roam-hierarchy)
                      " * "))
            (if (org-roam-node-refs node)
                (propertize
                 (org-roam-node-title node)
                 'face 'mymy-org-roam-ref-face)
              (org-roam-node-title node))))
  ;; End of second dailies

  (emacsql-fix-vector-indentation)
  (defun mymy-org-roam-get-backlinks (&optional id)
    (interactive)
    (caar (org-roam-db-query
           [:select (funcall count source)
            :from links
            :where (= dest $s1)
            :and (= type "id")]
           (or id (org-id-get)))))
  ;; (org-roam-node-backlink-count (org-roam-populate (org-roam-node-create :id (org-id-get))))

  (cl-defmethod org-roam-node-backlinkcount ((node org-roam-node))
    (format "[%d]" (org-roam-node-backlink-count node)))

  (setq mymy-org-roam-project-template '(("p" "project" plain "%?"
                                          :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: TODO\n\n* ${title}")
                                          :unnarrowed t)))
  (add-to-list 'helm-completing-read-handlers-alist
                '(org-roam-node-find . helm-completing-read-sync-default-handler))
  (add-to-list 'helm-completing-read-handlers-alist
                '(mymy-org-roam-node-insert-wrapper . helm-completing-read-sync-default-handler))
  ;; (add-to-list 'helm-completing-read-handlers-alist
  ;;              '(org-roam-node-insert . helm-completing-read-sync-default-handler))
  (defface mymy-org-roam-title
    '((((class color) (background light)) (:underline t :bold t))
      (((class color) (background dark)) (:underline t :bold t))
      (t (:underline t :bold t)))
    "Face for node find tittle."
    :group 'org-faces)
  (defface mymy-org-roam-tag '((t :inherit org-warning))
    "Face for tags"
    :group 'org-faces)

  ;; Copied from nobreak-space
  (defface mymy-org-roam-hierarchy
    '((((class color) (min-colors 88)) :inherit escape-glyph :underline t)
      (((class color) (min-colors 8)) :background "magenta")
      (t :inverse-video t))
    "Face for hierarchy"
    )

  (defface mymy-org-roam-ref-face
    '((t (:foreground "Dark Orange" :bold t)))
    "org-roam-ref-face face.")

  ;; Served its purpose
  ;; (defun move-to-notes ()
  ;;     (interactive)
  ;;     (let ((old-roam-dir (concat (expand-file-name main-dropbox-dir) "org-roam"))
  ;;           (org-roam-directory (concat main-dropbox-dir "Notes"))
  ;;           )
  ;;       (if (and (string-prefix-p old-roam-dir (buffer-file-name))
  ;;                (not (file-exists-p (concat (expand-file-name org-roam-directory) (substring (buffer-file-name) (length old-roam-dir))))))
  ;;           (progn
  ;;             (message (shell-command-to-string
  ;;                       (format
  ;;                        "git log --pretty=email --patch-with-stat --reverse -- \"%s\" \
  ;; | (cd \"%s\" && git am --committer-date-is-author-date)"
  ;;                        (buffer-file-name) (expand-file-name org-roam-directory))))
  ;;             (message "Finished"))
  ;;         (message "File already exists or not in org roam dir"))))

  ;; Served its purpose as a prototype
  ;; (defun mymy-insert-exact-date-in-unix-as-link ()
  ;;   (interactive)
  ;;   (let ((unix-date (format-time-string "%s")))
  ;;     (insert (format "<<%s>>" unix-date))
  ;;     (kill-new (format "[[%s]]" unix-date))))

  (defun mymy-insert-unix-date ()
    (interactive)
    (insert (format-time-string "%s")))

  ;; To not forget this exists
  (setq mymy-org-roam-entry-separation-tag ",")

  (defun org-roam-node-insert-immediate (arg &optional args)
    (interactive "P")
    ;; Refactored after watching system crafters video
    (let ((args (cons arg args))
          (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                    '(:immediate-finish t)))))
      (apply #'org-roam-node-insert args)))
  (defun mymy-org-move-next-heading (&optional arg)
    (interactive)
    (org-hide-entry)
    (org-forward-heading-same-level (or arg 1))
    (org-show-entry))
  (defun mymy-org-roam-get-link ()
    "Insert node at point as a link in the kill ring"
    (interactive)
    (let* ((node (org-roam-node-at-point))
           (node-id (org-roam-node-id node))
           (node-name (org-roam-node-title node))
           (node-name (or node-name (org-entry-get nil "ITEM"))))
      (kill-new (format "[[id:%s][%s]]" node-id node-name))))

  (defun mymy-org-roam-insert-next-heading (&optional arg)
    (interactive)
    (save-excursion
      (org-forward-heading-same-level (or arg 1))
      (mymy-org-roam-get-link)
      )
    (yank))

  (defun mymy-org-id-get-create-with-roam-exclude (&optional force)
    (interactive)
    (org-entry-put (point) "ROAM_EXCLUDE" "t")
    (org-id-get-create force)
    )
  ;; my org roam preview at point
  (defun mymy-org-id-open-without-push-mark (id)
    "Slight modification to org-id-open"
    (let ((m (org-id-find id 'marker)))
      (unless m
        (error "Cannot find entry with ID \"%s\"" id))
      (if (not (equal (current-buffer) (marker-buffer m)))
          (funcall 'switch-to-buffer (marker-buffer m))
        )
      (goto-char m)
      (move-marker m nil)
      (org-show-context)
      )
    )
  (el-patch-defun org-roam-id-open (id _)
    "Go to the entry with id ID.
Like `org-id-open', but additionally uses the Org-roam database."
    (org-mark-ring-push)
    (let ((m (or (org-roam-id-find id 'marker)
                 (org-id-find id 'marker)))
          cmd)
      (unless m
        (error "Cannot find entry with ID \"%s\"" id))
      ;; Use a buffer-switching command in analogy to finding files
      (setq cmd
            (or
             (cdr
              (assq
               (cdr (assq 'file org-link-frame-setup))
               '((find-file . switch-to-buffer)
                 (find-file-other-window . switch-to-buffer-other-window)
                 (find-file-other-frame . switch-to-buffer-other-frame))))
             'switch-to-buffer-other-window))
      (if (not (equal (current-buffer) (marker-buffer m)))
          (funcall cmd (marker-buffer m)))
      (goto-char m)
      (move-marker m nil)
      (org-show-context)
      (el-patch-add (when (org-current-level) (org-show-entry)))))

  (defun mymy-org-roam-preview-node ()
    (let* ((context (org-element-context))
           (type (org-element-type context))
           ;; (beg (org-element-property :begin context))
           ;; (end (org-element-property :end context))
           (link-type (org-element-property :type context))
           (link (org-element-property :raw-link context))
           (id (org-element-property :path context)))
      (when (and (eq type 'link) (string-equal link-type "id"))
        ;; Never forget then they or you are in the same file
        (save-excursion
          (if  (< 0 (caar (org-roam-db-query [:select level :from nodes :where (= id $s1)] id)))
              ;; Headline
              (save-window-excursion
                (mymy-org-id-open-without-push-mark id)
                (let* ((headline-context (org-element-context))
                       (beg (org-element-property :begin headline-context))
                       (end (org-element-property :end headline-context)))
                  (buffer-substring-no-properties beg end)
                  )
                )
            ;; File
            (save-window-excursion
              (mymy-org-id-open-without-push-mark id)
              ;; Will leave it like that for some time
              (let ((beg (progn (org-roam-end-of-meta-data t)
                                (point)))
                    (end (progn (unless (eq 'headline (org-element-type (org-element-context)))
                                  (org-next-visible-heading 1))
                                (point))))
                (string-trim (buffer-substring-no-properties beg end)))))))))

  ;; Could be anywhere
  (setq org-tags-column 0)

  (defun mymy-org-roam-show-preview ()
    (interactive)
    (when-let ((node-preview (mymy-org-roam-preview-node)))
      (when (string-empty-p node-preview)
        (error "Nothing to show")
        )
      (let ((buf (get-buffer-create "*mymy-org-roam-preview*")))
        (with-current-buffer buf
          (org-mode)
          (erase-buffer)
          (insert node-preview)
          (goto-char (point-min))
          (org-show-all))
        (posframe-show buf
                       :poshandler 'posframe-poshandler-frame-top-right-corner
                       :hidehandler 'mymy-posframe-hidehandler-org-roam-hide-preview
                       :border-width 1 :border-color "blue"))))
  (defun mymy-posframe-hidehandler-org-roam-hide-preview (info)
    (and (not (string-equal "id" (org-element-property :type (org-element-context))))
         (get-buffer (cdr (plist-get info :posframe-parent-buffer)))))

  (setq org-insert-heading-respect-content nil)
  (defun mymy-org-roam-append-node-heading ()
    (interactive)
    (org-insert-heading-respect-content)
    (mymy-insert-unix-date)
    (org-id-get-create)
    (next-line)
    (goto-char (org-element-property :end (org-element-context)))
    (newline)
    (previous-line)
    )


  (defun org-count-subentries (&optional pos match scope level)
    "Return number of subentries for entry at POS.
MATCH and SCOPE are the same as for `org-map-entries', but
SCOPE defaults to 'tree.
By default, all subentries are counted; restrict with LEVEL."
    (interactive)
    (save-excursion
      (goto-char (or pos (point)))
      ;; If we are in the middle ot an entry, use the current heading.
      (org-back-to-heading t)
      (let ((maxlevel (when (and level (org-current-level))
                        (+ level (org-current-level)))))
        (message "%s subentries"
                 (1- (length
                      (delq nil
                            (org-map-entries
                             (lambda ()
                               ;; Return true, unless below maxlevel.
                               (or (not maxlevel)
                                   (<= (org-current-level) maxlevel)))
                             match (or scope 'tree)))))))))

  (defun mymy-org-move-prev-heading ()
    (interactive)
    (mymy-org-move-next-heading -1))
  ;; (setq org-indirect-buffer-display 'dedicated-frame)
  (setq org-indirect-buffer-display 'new-frame)

  (defun mymy-org-id-roam-create ()
    (interactive)
    (org-id-get-create)
    (mymy-org-roam-update-headline)
    )
  (defun mymy-say-back-link-count ()
    (interactive)
    (message (number-to-string (mymy-org-roam-get-backlinks)))
    )
  (defhydra hydra-zettel (:hint nil :color pink)
    "
_n_: Next _k_: Get link
_u_: Prev _o_: Open
_b_: Backlink count _a_: Append node
_._: Quit _q_: Quit
"
    ("n" mymy-org-move-next-heading)
    ("u" mymy-org-move-prev-heading)
    ("k" mymy-org-roam-get-link)
    ("a" mymy-org-roam-append-node-heading)
    ("o" (progn
           (org-tree-to-indirect-buffer)
           (visual-line-mode)))
    ("b" (message (number-to-string (mymy-org-roam-get-backlinks))))
    ("." nil :color blue)
    ("q" nil :color blue))
  (ryo-modal-keys
   ("S"
    (("l" org-roam-buffer-toggle)
     ("y"
      (("n" mymy-org-move-next-heading)
       ("u" mymy-org-move-prev-heading)
       ("a" mymy-org-roam-append-node-heading)
       ("k" mymy-org-roam-get-link)
       ("y" hydra-zettel/body)
       ("e" mymy-org-roam-insert-next-heading)
       ("b" mymy-say-back-link-count)
       )
      )
     ("k"
      (("f" org-roam-dailies-goto-today)
       ("F" mymy-org-roam-dailies-secondary-goto-today)
       ))
     ("n"
      (("n" org-roam-node-find)
       ("i" org-roam-node-insert)
       ("e" org-roam-node-insert-immediate)
       ("x" org-roam-extract-subtree)
       ;; ("b" move-to-notes)
       )
      )
     ("g" org-roam-graph)
     ("t"
      (("t" org-roam-tag-add)
       ("d" org-roam-tag-remove)))
     ("c" org-roam-dailies-capture-today)
     ("u" mymy-org-roam-show-preview)
     ;; ("u" my/org-roam-find-project)
     ("e"
      (("e" org-roam-ref-add)
       ("d" org-roam-ref-remove)
       ("n" mymy-org-roam-ref-find)
       ("b" helm-bibtex)
       ("i" org-ref-cite-insert-helm)
       ))
     ("o" mymy-org-id-roam-create)
     ("i"
      ;; (format-time-string "%c" (seconds-to-time 1645577664))
      (("i" mymy-insert-unix-date)
       ;; ("e" mymy-insert-exact-date-in-unix-as-link)
       ))
     ("r" mymy-refile-to-done))))
  (which-key-add-key-based-replacements
    "Sk" "Dailies"
    "Se" "Refs & bibtex"
    "Sn" "org-roam node"
    "St" "org-roam tags"
    )
  :hook
  (after-init . winner-mode)
  ;; (org-mode . org-hide-properties)
  :custom
  (org-roam-graph-filetype "svg")
  (org-roam-graph-executable "dot") ;; Never use neato unless you are going to keep 100 notes or less
  (org-roam-graph-viewer "/usr/bin/google-chrome-stable")
  (org-roam-graph-node-extra-config
   '(("id"
      ("style" . "bold,rounded,filled")
      ("fillcolor" . "#000000")
      ("color" . "#15FF00")
      ("fontcolor" . "#00FFFF")
      )
     ("nodesep" . "0.5")
     )
   )
  (org-roam-graph-edge-extra-config
   '(("dir" . "forward")
     ("weight" . "3")
     )
   )
  (org-roam-graph-extra-config
   '(("bgcolor" . "snow2") ;; https://graphviz.org/doc/info/colors.html
     ;; ("rank" . "source")
     ("ordering" . "out")
     ("rankdir" . "TB") ;; TBLR Top Bottom Left Rigth
     ("ranksep" . "1")  ;; https://graphviz.org/docs/attrs/ranksep/
     ("sep" . "4")      ;; Always bigger than esep
     ("esep" . "3")     ;; https://graphviz.org/docs/attrs/esep/
     )
   )
  (org-roam-graph-link-hidden-types '("file" "https" "fuzzy" "http" "gnus"))
  (org-roam-graph-shorten-titles 'wrap)
  (org-roam-graph-max-title-length '15)
  (org-roam-node-display-template
   (concat
    "${backlinkcount}"
    (propertize "${hierarchy}" 'face 'mymy-org-roam-title)
    " "
    (propertize "(${tags})" 'face 'mymy-org-roam-tag)
    ))
  (org-roam-completion-everywhere t)
  (org-tags-exclude-from-inheritance '("Core" "TODO"))
  ;; Deleted
  ;; (org-roam-buffer-window-parameters '((no-delete-other-windows . t)))
  ;; (org-roam-directory (concat main-dropbox-dir "org-roam"))
  (org-roam-directory mymy-org-roam-dir)
  ;; Daily notes
  (org-roam-dailies-directory "inbox/")
  (org-roam-extract-new-file-path "%<%Y-%m-%d-%H-%M-%S>-${slug}.org")
  (org-roam-capture-templates
   `(("d" "default" plain "%?"
      :target (file+head "%<%Y-%m-%d-%H-%M-%S>-${slug}.org" "#+title: ${title}\n\n* ${title}")
      :unnarrowed t)
     ("r" "bibliopraphic reference" entry "* lit %<%s>%?
:PROPERTIES:\n:ID:         %(org-id-new)
:ROAM_REFS: [[cite:&%^{citekey}]]
:END:
Title: %^{title}
Date: %^{date}
Author: %^{author}
\n\n"
      :target (file+olp ,(concat org-roam-directory "2022-01-09-12-38-23-zettelkasten.org")
                        ("Zettelkästen" "lit notes"))
      ;; :unnarrowed t
      )))
  (org-roam-node-template-prefixes
   '(("tags" . "#") ("todo" . "t:"))
   )
  (org-roam-node-formatter nil)
  (org-roam-dailies-capture-templates
   ;; Capture file
   `(("q" "Quick! notes" entry "* %?\n\n"
      :target (file+olp "capture.org" ("Quick Notes")))

     ;; Projects file
     ("p" "Projects" entry "* TODO %?\n"
      :prepend t
      :target (file+olp ,(concat org-roam-directory "Projects/20210715113548-projects.org")
                        ("Projects" "Inbox")))
     ("b" "Buffer" entry "* TODO Read [[%L][S]] %?\n"
      :prepend t
      :target (file+olp ,(concat org-roam-directory "20210715113548-projects.org")
                        ("Projects" "Stack")))

     ;; New words
     ("e" "English" entry "* %?\n\n"
      :target (file+olp ,(concat org-roam-directory "2021-07-19-13-58-56-english.org")
                        ("Dictionary"))
      )
     ))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ;; Dailies
   ("C-c C-j" . mymy-org-roam-dailies-secondary-capture-today)
   :map org-mode-map
   ("C-c C-j" . mymy-org-roam-dailies-secondary-capture-today)
   ))
(use-package org-transclusion)
(use-package org-roam-bibtex
  :config
  (setq orb-roam-ref-format 'org-ref-v3)
  (org-roam-bibtex-mode)
  (setq orb-preformat-templates t)
  (setq orb-preformat-keywords '("title" "citekey" "entry-type" "date" "author"))
  )
(use-package asoc
  :straight (asoc :type git :host github :repo "troyp/asoc.el")
  )
(use-package org-capture-ref
  :after org-roam
  :straight doct
  ;; :straight (asoc.el :type git :host github :repo "troyp/asoc.el")
  :straight (org-capture-ref :type git :host github :repo "yantar92/org-capture-ref")
  :config
  (gsetq org-capture-ref-capture-target (concat mymy-org-roam-dir "inbox/capture.org")
         org-capture-ref-capture-keys '("b" "B")
         org-capture-ref-capture-template `( :group "Browser link"
                                             :type entry
                                             :file ,org-capture-ref-capture-target
                                             :fetch-bibtex (lambda () (org-capture-ref-process-capture)) ; this must run first
                                             :link-type (lambda () (org-capture-ref-get-bibtex-field :type))
                                             :extra (lambda () (if (org-capture-ref-get-bibtex-field :journal)
                                                                   (s-join "\n"
                                                                           '("- [ ] download and attach pdf"
                                                                             "- [ ] [[elisp:org-attach-open][read paper capturing interesting references]]"
                                                                             "- [ ] [[elisp:(browse-url (url-encode-url (format \"https://www.semanticscholar.org/search?q=%s\" (org-entry-get nil \"TITLE\"))))][check citing articles]]"
                                                                             "- [ ] [[elisp:(browse-url (url-encode-url (format \"https://www.connectedpapers.com/search?q=%s\" (org-entry-get nil \"TITLE\"))))][check related articles]]"
                                                                             "- [ ] check if bibtex entry has missing fields"))
                                                                 ""))
                                             :org-entry (lambda () (org-capture-ref-get-org-entry))
                                             :bibtex-string (lambda () (org-capture-ref-format-bibtex))
                                             :template
                                             ("%{fetch-bibtex}* TODO %?%{space}%{org-entry}"
                                              "%{extra}"
                                              "%{bibtex-string}"
                                              "- Keywords: #%{link-type}")
                                             :headline "References"
                                             :children (("Interactive link"
                                                         :keys ,(car org-capture-ref-capture-keys)
                                                         :clock-in t
                                                         :space " "
                                                         :clock-resume t
                                                         )
                                                        ("Silent link"
                                                         :keys ,(cadr org-capture-ref-capture-keys)
                                                         :space ""
                                                         :immediate-finish t))))
  (let ((templates (doct org-capture-ref-capture-template)))
    (dolist (template templates)
      (asoc-put! org-capture-templates
                 (car template)
                 (cdr  template)
                 'replace)))
  (setq org-capture-ref-capture-template-set-p t))

(use-package org-pomodoro
  ;; Works using org mode headings
  :config
  (ryo-modal-key "Sm" 'org-pomodoro)
  (setq alert-user-configuration '((((:category . "org-pomodoro")) libnotify nil)))
  (setq org-pomodoro-length 30
        org-pomodoro-short-break-length 6
        org-pomodoro-long-break-length 24)
  ;; (setq org-pomodoro-length 57
  ;;       org-pomodoro-short-break-length 11
  ;;       org-pomodoro-long-break-length 46)
  ;; (setq
  ;;  org-pomodoro-length 52
  ;;  org-pomodoro-short-break-length 17
  ;;  org-pomodoro-long-break-length 39
  ;;  )
  (defun ruborcalor/org-pomodoro-time ()
    "Return the remaining pomodoro time"
    (if (org-pomodoro-active-p)
        (cl-case org-pomodoro-state
          (:pomodoro
           (format "Pomo: %d minutes - %s" (/ (org-pomodoro-remaining-seconds) 60) org-clock-heading))
          (:short-break
           (format "Short break time: %d minutes" (/ (org-pomodoro-remaining-seconds) 60)))
          (:long-break
           (format "Long break time: %d minutes" (/ (org-pomodoro-remaining-seconds) 60)))
          (:overtime
           (format "Overtime! %d minutes" (/ (org-pomodoro-remaining-seconds) 60))))
      "No active pomo"))
  :hook
  (org-pomodoro-short-break-finished . (lambda () (interactive) (org-pomodoro '(16)))))

;; (use-package org-hyperscheduler)

(use-package eww
  :config
  (bind-key "u" nil eww-link-keymap)
  (bind-key "u" nil eww-image-link-keymap)
  (setq shr-width 80)
  :hook
  ((eww-mode . olivetti-mode))
  )
(use-package gnus
  :config
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
  (add-hook 'gnus-summary-mode-hook #'gnus-undo-mode)
  (setq gnus-asynchronous t)
  (setq gnus-buffer-configuration '((group
                                     (frame 1.0
                                            (group 1.0 point)))
                                    (summary
                                     (frame 1.0
                                            (summary 1.0 point)))
                                    (article
                                     (cond
                                      (gnus-use-trees
                                       '(frame 1.0
                                               (summary 0.25 point)
                                               (tree 0.25)
                                               (article 1.0)))
                                      (t
                                       '(frame 1.0
                                               (summary 0.25 point)
                                               (article 1.0)))))
                                    (server
                                     (frame 1.0
                                            (server 1.0 point)))
                                    (browse
                                     (frame 1.0
                                            (browse 1.0 point)))
                                    (message
                                     (frame 1.0
                                            (message 1.0 point)))
                                    (pick
                                     (frame 1.0
                                            (article 1.0 point)))
                                    (info
                                     (frame 1.0
                                            (info 1.0 point)))
                                    (summary-faq
                                     (frame 1.0
                                            (summary 0.25)
                                            (faq 1.0 point)))
                                    (only-article
                                     (frame 1.0
                                            (article 1.0 point)))
                                    (edit-article
                                     (frame 1.0
                                            (article 1.0 point)))
                                    (edit-form
                                     (frame 1.0
                                            (group 0.5)
                                            (edit-form 1.0 point)))
                                    (edit-score
                                     (frame 1.0
                                            (summary 0.25)
                                            (edit-score 1.0 point)))
                                    (edit-server
                                     (frame 1.0
                                            (server 0.5)
                                            (edit-form 1.0 point)))
                                    (post
                                     (frame 1.0
                                            (post 1.0 point)))
                                    (reply
                                     (frame 1.0
                                            (article 0.5)
                                            (message 1.0 point)))
                                    (forward
                                     (frame 1.0
                                            (message 1.0 point)))
                                    (reply-yank
                                     (frame 1.0
                                            (message 1.0 point)))
                                    (mail-bounce
                                     (frame 1.0
                                            (article 0.5)
                                            (message 1.0 point)))
                                    (pipe
                                     (frame 1.0
                                            (summary 0.25 point)
                                            ("*Shell Command Output*" 1.0)))
                                    (bug
                                     (frame 1.0
                                            ("*Gnus Bug*" 1.0 point)))
                                    (score-trace
                                     (frame 1.0
                                            (summary 0.5 point)
                                            ("*Score Trace*" 1.0)))
                                    (score-words
                                     (frame 1.0
                                            (summary 0.5 point)
                                            ("*Score Words*" 1.0)))
                                    (split-trace
                                     (frame 1.0
                                            (summary 0.5 point)
                                            ("*Split Trace*" 1.0)))
                                    (category
                                     (frame 1.0
                                            (category 1.0)))
                                    (compose-bounce
                                     (frame 1.0
                                            (article 0.5)
                                            (message 1.0 point)))
                                    (display-term
                                     (frame 1.0
                                            ("*display*" 1.0)))
                                    (mml-preview
                                     (frame 1.0
                                            (message 0.5)
                                            (mml-preview 1.0 point)))))
  )
(use-package deft
  :custom
  (deft-extensions '("org"))
  (deft-directory (concat main-dropbox-dir "org-roam/"))
  (deft-recursive t)
  )
(use-package anki-editor
  :disabled
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
(use-package org-drill :disabled)
(use-package quickrun)
(use-package olivetti :straight t)
(use-package nov
  :disabled
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :hook
  ((nov-mode . olivetti-mode)
   (nov-mode . (lambda () (face-remap-add-relative
                           'variable-pitch
                           :family "Liberation Serif"
                           :height 1.0)))
   (nov-mode . (lambda () (whitespace-mode -1)))
   )
  :custom
  (setq nov-text-width 60)
  )
(use-package projectile
  :diminish projectile-mode
  :init
  (general-define-key
   :keymaps 'projectile-mode-map
   "C-c t" 'projectile-command-map)
  (setq-default projectile-project-search-path '("~/Projects/"))
  (setq projectile-completion-system 'helm
        projectile-switch-project-action 'helm-projectile)
  :hook
  (after-init . projectile-mode)
  )
(use-package frames-only-mode
  :hook
  (after-init . frames-only-mode)
  :init
  (setq frames-only-mode-kill-frame-when-buffer-killed-buffer-list
        ;; Default
        '("*RefTeX Select*" "*Help*" "*Popup Help*" "*Completions*"
          ;; Cider group
          "*cider-doc*" "*cider-error*" "*cider-result*" "*cider-apropos*"
          ;; Vterm
          "*vterm*"
          ;; Poetry
          "*poetry*"
          )
        )
  ;; :config
  ;; (advice-add 'org-roam-dailies-capture-today :around 'frames-only-mode-advice-use-windows)
  )
(use-package avy
  :bind(("M-g g" . avy-goto-line)
        ("M-g M-g" . avy-goto-line)
        )
  :init
  (setq avy-case-fold-search nil)
  (setq avy-keys '(?n ?e ?i ?k ?y ?m ?u ?c ?r ?s ?t))
  :config
  (set-face-attribute 'avy-lead-face nil
                      :background "#818182" :foreground "#000000")
  (set-face-attribute 'avy-lead-face-0 nil
                      :background "#bdbca6" :foreground "#000000")
  )
(use-package ace-window
  :config
  (setq aw-keys '(?n ?e ?i ?o ?k ?m ?u ?y))
  (ace-window-display-mode)
  )
(use-package windmove
  :config
  (windmove-default-keybindings)
  (setq windmove-wrap-around t))

(use-package yasnippet
  :bind (("C-l" . yas-expand)
         )
  :config
  (add-to-list 'company-backends 'company-yasnippet t)
  ;; (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  (defun mymy-yasnippet-hook ()
    (yas-define-snippets
     'emacs-lisp-mode
     '(;; "Key" "Template" "Name"
       ("up" "(use-package ${1:package-name}$0)" "use-package")
       (":c" ":config" "Use package :config")
       (":i" ":init" "Use package :init")
       (":b" ":bind (($1))" "Use package :bind")
       (":bd" "(\"${1:keys}\" . ${2:function})" "Use package :bind binding")
       (":s" ":straight " "Use package :straigth")
       (":ss" ":straight (:host github :type git :repo \"${1:repo}\" :files (${2:files}))" ":straight repo")
       (":h" ":hook" "Use package :hook")
       )
     )
    (yas-define-snippets
     'python-mode
     '(;; "Key" "Template" "Name"
       ("pr" "print(${1:`(when (region-active-p) (yas-selected-text))`})$0" "print")
       )
     )
    (yas-define-snippets
     'org-mode
     '(;; "Key" "Template" "Name"
       ("clj" "#+begin_src clojure\n\n#+end_src\n" "Clojure source block")
       ("<co" "#+STARTUP: content" "Content")
       ("<n" "#+STARTUP: nofold" "No fold")

       (":el" "\\begin{${1:env}}\n\\end{${1:$(yas-field-value 1)}}" "Enviroment")
       (":a" "align" "Aling env")
       (":a*" "align*" "Align* env")
       (":e" "equation" "Equation env")
       (":e*" "equation*" "Equation* env")

       ("::frac" "\\frac{${1:a}}{${2:b}}" "Fraction")
       ("::div" "{${1:a} \\div ${2:b}}" "Division")
       ("::cdot" "{${1:a} \\cdot ${2:b}}" "Multiplication")
       ("::=" "{${1:a} = ${2:b}}" "Equality")
       ("::nck" "{${1:n} \\choose ${2:k}}" "Choose function")
       ("::int" "\\int_{${1:a}}^{${2:b}} ${3:f(x)} \\,dx" "Integrals")
       ("::sum" "\\sum_{${1:a}}^{${2:b}}" "Summation")
       )
     )
    )
  :bind ((;; C-c y is reserved for yasnippets and, possibly, its
          ;; snippets shortcuts
          "C-c y y" . company-yasnippet)
         )
  :hook
  ;; Doesn't work for some reason
  ;; (after-init . yas-global-mode)
  ;; (yas-global-mode-hook . mymy-yasnippet-hook)
  (prog-mode . yas-minor-mode)
  (text-mode . yas-minor-mode)
  ;; I'm really getting sure this things run
  (yas-minor-mode . mymy-yasnippet-hook)
  )
(use-package slime-company
  :after (slime company)
  :config (setq slime-company-completion 'fuzzy
                slime-company-after-completion 'slime-company-just-one-space))
(use-package slime
  :init
  (setq slime-lisp-implementations
        '((sbcl . "/usr/bin/sbcl")))
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (slime-setup '(slime-company
                 slime-fancy
                 slime-asdf
                 slime-autodoc
                 slime-editing-commands
                 slime-fancy-inspector
                 slime-fontifying-fu
                 slime-fuzzy
                 slime-indentation
                 slime-mdot-fu
                 slime-package-fu
                 slime-references
                 slime-repl
                 slime-sbcl-exts
                 slime-scratch
                 slime-xref-browser))
  (slime-autodoc-mode)
  (setq slime-complete-symbol*-fancy t)
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
  :hook
  (lisp-mode . (lambda () (setq-local company-backends (cons 'company-slime company-backends))))
  (lisp-mode . aggressive-indent-mode)
  )

(use-package dashboard
  :straight t
  :straight all-the-icons page-break-lines
  :init
  ;; Set the banner
  ;; (setq dashboard-startup-banner [VALUE])
  (dashboard-setup-startup-hook)
  (setq dashboard-set-footer nil)
  (setq dashboard-show-shortcuts t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t)
  ;; Projects, Areas of activity, Reading list
  (setq dashboard-items '((bookmarks . 5)
                          (projects . 5)
                          ))
  :bind (:map dashboard-mode-map
              ("TAB" . dashboard-next-section)
              ("<backtab>" . dashboard-previous-section))
  :config ;; For now i'm going to try something
  (setq inhibit-startup-screen t)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  ;; (setq initial-buffer-choice (concat org-roam-directory "Projects/20210715113548-projects.org"))
  (bookmark-location (car bookmark-alist))
  (el-patch-defun dashboard-insert-bookmarks (list-size)
    "Add the list of LIST-SIZE items of bookmarks."
    (require 'bookmark)
    (dashboard-insert-section
     "Bookmarks:"
     (dashboard-subseq (bookmark-all-names) 0 list-size)
     list-size
     (dashboard-get-shortcut 'bookmarks)
     (el-patch-swap `(lambda (&rest ignore) (bookmark-jump ,el))
                    `(lambda (&rest ignore) (find-file (bookmark-location ,el))))
     (if-let* ((filename el)
               (path (bookmark-get-filename el))
               (path-shorten (dashboard-shorten-path path 'bookmarks)))
         (cond
          ((eq dashboard-bookmarks-show-base 'align)
           (unless dashboard--bookmarks-cache-item-format
             (let* ((len-align (dashboard--align-length-by-type 'bookmarks))
                    (new-fmt (dashboard--generate-align-format
                              dashboard-bookmarks-item-format len-align)))
               (setq dashboard--bookmarks-cache-item-format new-fmt)))
           (format dashboard--bookmarks-cache-item-format filename path-shorten))
          ((null dashboard-bookmarks-show-base) path-shorten)
          (t (format dashboard-bookmarks-item-format filename path-shorten)))
       el))))

(use-package undo-tree
  :ryo
  ("q /" undo-tree-visualize)
  ("?" undo-tree-redo)
  :bind
  (:map undo-tree-visualizer-mode-map
        ("q" . undo-tree-visualizer-quit)
        ("N" . undo-tree-visualize-switch-branch-left)
        ("n" . undo-tree-visualize-redo)
        ("u" . undo-tree-visualize-undo)
        ("U" . undo-tree-visualize-switch-branch-right)))
(use-package selected ;; Special keybindings when a region is active
  :straight t
  ;; selected-<major-mode>-map for major mode specific keybindings
  ;; (setq selected-org-mode-map (make-sparse-keymap)) example org mode
  :commands selected-minor-mode
  :config
  (setq selected-minor-mode-override t)
  :bind (:map selected-keymap
              ("q" . selected-off)
              ("" . count-words-region))
  )
(use-package ace-mc)
(use-package direx)
(use-package nyan-mode ;; Nyan, simple nyan
  :diminish nyan-mode
  :init
  (setq nyan-animate-nyancat t
        nyan-wavy-trail t)
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
        centaur-tabs-label-fixed-length 6
        )
  (setq uniquify-separator "/"
        uniquify-buffer-name-style 'forward)
  (centaur-tabs-group-by-projectile-project)
  ;; (setq helm-source-centaur-tabs-group
  ;;       (helm-build-sync-source "Centaur tabs groups"
  ;;         :candidates #'centaur-tabs-get-groups
  ;;         :action '(("Switch to group" . centaur-tabs-switch-group))))
  ;; (add-to-list 'helm-mini-default-sources 'helm-source-centaur-tabs-group)
  (centaur-tabs-get-groups)
  :bind
  ("C-S-<iso-lefttab>" . centaur-tabs-backward)
  ("C-<tab>" . centaur-tabs-forward)
  ("C-z" . centaur-tabs-backward-group)
  ("C-S-z" . centaur-tabs-forward-group)
  )
(use-package which-key ;; Useful to tell what is the next command that i can do
  :init
  (setq which-key-enable-extended-define-key t)
  (setq which-key-side-window-location 'bottom)
  (setq which-key-side-window-max-height 0.40)
  (setq which-key-side-window-max-width 0.66)
  :config
  (push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist)
  (which-key-mode))
(use-package expand-region
  :bind (("C-'" . er/expand-region)))
(use-package yequake
  :after org-roam
  :custom
  (yequake-frames
   '(("org-roam-dailies-capture-today"
      (buffer-fns . (yequake-org-roam-dailies-capture-today))
      (width . 0.75)
      (height . 0.5)
      (alpha . 0.80)
      (frame-parameters . ((undecorated . t)
                           (sticky . t)
                           (skip-taskbar . t))))
     )
   )
  )

(use-package dired
  :straight nil
  :bind (:map dired-mode-map
              ("." . hydra-dired/body))
  :init
  (defhydra hydra-dired (:hint nil :color pink)
    "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
    ("\\" dired-do-ispell)
    ("(" dired-hide-details-mode)
    (")" dired-omit-mode)
    ("+" dired-create-directory)
    ("=" diredp-ediff) ;; smart diff
    ("?" dired-summary)
    ("$" diredp-hide-subdir-nomove)
    ("A" dired-do-find-regexp)
    ("C" dired-do-copy) ;; Copy all marked files
    ("D" dired-do-delete)
    ("E" dired-mark-extension)
    ("e" dired-ediff-files)
    ("F" dired-do-find-marked-files)
    ("G" dired-do-chgrp)
    ("g" revert-buffer) ;; read all directories again (refresh)
    ("i" dired-maybe-insert-subdir)
    ("l" dired-do-redisplay) ;; relist the marked or singel directory
    ("M" dired-do-chmod)
    ("m" dired-mark)
    ("O" dired-display-file)
    ("o" dired-find-file-other-window)
    ("Q" dired-do-find-regexp-and-replace)
    ("R" dired-do-rename)
    ("r" dired-do-rsynch)
    ("S" dired-do-symlink)
    ("s" dired-sort-toggle-or-edit)
    ("t" dired-toggle-marks)
    ("U" dired-unmark-all-marks)
    ("u" dired-unmark)
    ("v" dired-view-file) ;; q to exit, s to search, = gets line #
    ("w" dired-kill-subdir)
    ("Y" dired-do-relsymlink)
    ("z" diredp-compress-this-file)
    ("Z" dired-do-compress)
    ("q" nil)
    ("." nil :color blue))
  )
(use-package straight
  :straight nil
  :bind (("C-c s t" . hydra-straight-helper/body))
  :ryo
  ("hs" hydra-straight-helper/body)
  (defhydra hydra-straight-helper (:hint nil)
    "
_c_heck all       |_f_etch all     |_m_erge all      |_n_ormalize all   |p_u_sh all
_C_heck package   |_F_etch package |_M_erge package  |_N_ormlize package|p_U_sh package
----------------^^+--------------^^+---------------^^+----------------^^+------------||_q_uit||
_r_ebuild all     |_p_ull all      |_v_ersions freeze|_w_atcher start   |_g_et recipe
_R_ebuild package |_P_ull package  |_V_ersions thaw  |_W_atcher quit    |prun_e_ build"
    ("c" straight-check-all)
    ("C" straight-check-package)
    ("r" straight-rebuild-all)
    ("R" straight-rebuild-package)
    ("f" straight-fetch-all)
    ("F" straight-fetch-package)
    ("p" straight-pull-all)
    ("P" straight-pull-package)
    ("m" straight-merge-all)
    ("M" straight-merge-package)
    ("n" straight-normalize-all)
    ("N" straight-normalize-package)
    ("u" straight-push-all)
    ("U" straight-push-package)
    ("v" straight-freeze-versions)
    ("V" straight-thaw-versions)
    ("w" straight-watcher-start)
    ("W" straight-watcher-quit)
    ("g" straight-get-recipe)
    ("e" straight-prune-build)
    ("q" nil))
  )

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setenv "JAVA_HOME"  "/usr/lib/jvm/java-11-openjdk/")
(setenv "PATH" (let ((current (getenv "PATH"))
                     (new (concat (getenv "HOME") "/.local/bin")))
                 (if current (concat new ":" current) new)))
(setq lsp-java-java-path (concat (getenv "JAVA_HOME") "bin/java"))
(setq default-fill-column 74)
;; To remind me that this thing is posible
;; I yet don't feel worthy of such power
;; (setq enable-recursive-minibuffers t)

;; Why is 60 the default? Not because is such a low number but because
;; Is not a power of 2
(setq kill-ring-max 1024)

;; Emacs, why is this so LOW
(setq mark-ring-max (* 1024 8))

;; Is better to accept that in reality, i don't need this
(setq line-move-visual nil)

(setq highlight-nonselected-windows t)
(setq use-dialog-box nil) ; Text-based options are better
(setq bidi-display-reordering nil)
;; Why this varible even exists?
(setq delete-pair-blink-delay 0)
(setq font-lock-verbose nil)
(setq byte-compile-verbose nil)
(setq default-tab-width 4)
(setq tab-width 4)
(setq inhibit-compacting-font-caches t)
(setq custom-file "~/.emacs.d/custom-file.el")
(setq use-package-always-demand (daemonp))
(setq mouse-yank-at-point t)
(setq comp-async-report-warnings-errors nil)
(if (s-suffix\? "laptop" (system-name))
    (setq default-input-method "japanese")
  (setq default-input-method "japanese-mozc"))
(setq-default tab-always-indent t)
(setq-default whitespace-line-column 1000)
(setq-default cursor-type '(bar . 2)) ;; Change cursor to a bar
(setq-default cursor-in-non-selected-windows nil)
(setq-default blink-cursor-blinks 0) ; Never stop to blink
(setq-default frame-title-format "%b %& emacs")
(setq-default garbage-collection-messages t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq scroll-error-top-bottom t)
(setq-default
 prettify-symbols-alist
 '(("#+BEGIN_SRC"     . "Λ")
   ("#+END_SRC"       . "Λ")
   ("#+begin_src"     . "Λ")
   ("#+end_src"       . "Λ")
   ("lambda"          . "λ")
   ))
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
;; (setq auto-save-default nil)
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

(setq exec-path (append exec-path '("~/.local/bin")))
(setq jit-lock-defer-time 0)
(setq fast-but-imprecise-scrolling t)
(setq auto-window-vscroll nil)
(setq savehist-additional-variables '(register-alist))
(setq
 split-height-threshold 4
 split-width-threshold 40
 split-window-preferred-function 'split-window-really-sensibly)

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

;;                   Don't know where to put it, but it's still config
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

(setq custom--inhibit-theme-enable nil)

(defface font-lock-operator-face
  '((t :foreground "#8fff9e")) "Basic face for operator."
  :group 'basic-faces)
(defface font-lock-number-face
  '((t :foreground "#f56356")) "Basic face for number."
  :group 'basic-faces)



;; (add-hook 'window-configuration-change-hook
;;           (lambda ()
;;             (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) 1 1)))

(add-hook 'csv-mode-hook
          'csv-align-mode
          )

(add-hook 'after-init-hook
          (lambda()
            (which-function-mode t)
            (global-undo-tree-mode)
            ))
(add-hook 'prog-mode
          (lambda()
            (auto-fill-mode t)
            )
          )
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

;; MyMy mode

(defun increment-number-at-point ()
  (interactive)
  (let ((old-point (point)))
    (unwind-protect
        (progn
          (skip-chars-backward "0-9")
          (or (looking-at "[0-9]+")
              (error "No number at point"))
          (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))
      (goto-char old-point))))
(defun decrement-number-at-point ()
  (interactive)
  (let ((old-point (point)))
    (unwind-protect
        (progn
          (skip-chars-backward "0-9")
          (or (looking-at "[0-9]+")
              (error "No number at point"))
          (replace-match (number-to-string (1- (string-to-number (match-string 0))))))
      (goto-char old-point))))
(ryo-modal-keys
 ("z+" increment-number-at-point)
 ("z-" decrement-number-at-point)
 )
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
(add-hook 'after-init-hook 'mymy-global-mode)








































;; Load all functions
(require 'functions)
;; ;; Load the maped ryo-modal
(require 'MyMy-mode)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(ryo-modal-global-mode t)
(put 'dired-find-alternate-file 'disabled nil)

;; Init.el ends here
