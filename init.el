;; -*- lexical-binding: t; -*-

;;; Load before alpaca

;; Ensure loading of early-init
(when (version< emacs-version "27")
  (load (concat user-emacs-directory "early-init.el")))

;; Speed-up emacs startup
;; Reference: https://github.com/bkaestner/.emacs.d/blob/37c75bfe3a199594ad89504d870e68f6f424764f/early-init.el
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)
(defun my-cleanup-gc ()
  "Clean up gc."
  (setq gc-cons-threshold 16777216)
  (garbage-collect))

(add-hook 'emacs-startup-hook
          (lambda ()
            (progn
              (run-with-idle-timer 4 nil #'my-cleanup-gc)
              (setq gc-cons-threshold 16777216
                    gc-cons-percentage 0.1))))


;;; Elpaca paca boostrap

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
        ;; Enable use-package :ensure support for Elpaca.
        (elpaca-use-package-mode))

;; Make use package verbose on debug-init
(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

;;Turns off elpaca-use-package-mode current declaration
;;Note this will cause evaluate the declaration immediately. It is not deferred.
;;Useful for configuring built-in emacs features.
(use-package emacs
  :ensure nil
  :config
  (setq ring-bell-function #'ignore)
  (setq redisplay-dont-pause t)
  (setq frame-resize-pixelwise t)
  ;; Increase the amount of bytes that emacs can read from an extenarl process
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (tool-bar-mode -1)                           ; This is much easier
  (menu-bar-mode -1)                           ; than needing to change
  (scroll-bar-mode -1)                         ; this on every OS
  (setq byte-compile-warnings '(not obsolete)) ;; Cl warnings
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t)
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq create-lockfiles nil)
  (setq abbrev-suggest t)
  ;; I finally caught on. This is annoying when it tries to.
  (setq require-final-newline nil)
  (setq mode-require-final-newline nil)
  ;;; Set the font
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:family "Fantasque Sans Mono" :foundry "outline" :slant normal :weight normal :height 130 :width normal))))
   '(olivetti-fringe ((t (:foreground "#353535" :background "#353535")))))

  ;;; Load path

  (add-to-list 'load-path (concat user-emacs-directory "lib/"))

  (defcustom powerline-buffer-size-suffix t
    "Display the buffer size suffix."
    :group 'powerline
    :type 'boolean)

  (defun powerline-vc ()
    (when (and (buffer-file-name (current-buffer)) vc-mode)
      (format " %s%s"
              (char-to-string #xe0a0)
              (format-mode-line '(vc-mode vc-mode)))))

  (defun powerline-buffer-size ()
    (propertize
     (if powerline-buffer-size-suffix
         "%I"
       "%i")
     'mouse-face 'mode-line-highlight
     'local-map (make-mode-line-mouse-map
                 'mouse-1 (lambda () (interactive)
                            (setq powerline-buffer-size-suffix
                                  (not powerline-buffer-size-suffix))
                            (force-mode-line-update)))))



  (defun mode-line-align (left right)
    "Return a string with LEFT and RIGHT at the edges of the
current window."
    (format (format "%%s %%%ds" (- (window-total-width) (length left) 2))
            left right))

  ;;; Modeline
  (setq-default
   mode-line-format
   '((:eval
      (mode-line-align
       (format-mode-line
        (list " " (powerline-buffer-size)
              " "
              " " mode-line-buffer-identification
              " " mode-line-modified
              " "
              " " mode-line-percent-position "%%"
              " " "%n"
              " "
              ))
       (format-mode-line
        (list mode-name
              " " mode-line-misc-info
              (powerline-vc)
              ;; " " (format-time-string "%H:%M")
              ))))))

  (add-hook
   'after-init-hook
   ;; Darkula messing up with my modeline look
   (lambda ()
     (set-face-attribute
      'mode-line nil
      :overline "#EFEFF7"
      :box nil
      :underline nil
      :background "#2B2B2B")))



  ;;; Frame
  (add-to-list 'default-frame-alist
               '(internal-border-width . 20))

  (set-frame-parameter (selected-frame) 'internal-border-width 20)

  (add-to-list 'default-frame-alist
               '(alpha-background . 99))
  )

;; Add configuration which relies on after-init-hook, emacs-startup-hook,
;; etc to elpaca-after-init-hook so it runs after Elpaca has activated all
;; queued packages. This includes loading of saved customizations. e.g.

(add-hook 'elpaca-after-init-hook
          (lambda () (load custom-file 'noerror)))


;;; Emacs maintaining functionality
;; Here goes library installations, packages that help maintaining emacs

;; Explicit patching of functions and variables.
(use-package el-patch)

;; Bring a little bit of clojure and more
(use-package dash :config (global-dash-fontify-mode))

;;When installing a package used in the init file itself,
;;e.g. a package which adds a use-package key word,
;;use the :wait recipe keyword to block until that package is installed/configured.
;;For example:
(use-package general
  :ensure (:wait t)
  :demand t
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (defalias 'gsetq 'general-setq)
  (defalias 'gsetq-local 'general-setq-local)
  (defalias 'gsetq-default 'general-setq-default)
  ;; Activate general mode for overriding map
  (general-override-mode)
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
    "M-f"
    )

  (general-define-key
   :keymaps 'emacs-lisp-mode-map
   ;; I'm tired of this
   "C-c C-e" nil
   )
  )

;; Sync shell env variables to emacs env variables
(use-package exec-path-from-shell
  :config
  (add-to-list 'exec-path-from-shell-variables "ANDROID_HOME")
  (add-to-list 'exec-path-from-shell-variables "ANDROID_SDK_ROOT")
  (when (or (memq window-system '(mac ns x))
            (daemonp))
    (exec-path-from-shell-initialize)))

;;; EVIL
;; Expands to: (elpaca evil (use-package evil :demand t))
;; Make emacs vim
(use-package evil
  :config
  (evil-mode)
  (gsetq evil-undo-system 'undo-redo)
  (general-def 'normal emacs-lisp-mode-map
    "K" 'elisp-slime-nav-describe-elisp-thing-at-point)

  (general-override-mode 1)
  (define-key evil-emacs-state-map (kbd "S-SPC") 'evil-normal-state)

  ;; It get's kind of annoying, maybe will activate later.
  (gsetq evil-want-empty-ex-last-command nil)

  (general-define-key
   :states '(normal motion visual)
   :keymaps 'override
   :prefix "SPC"

   "s" #'save-buffer
   "b" #'switch-to-last-buffer
   "fb" #'consult-buffer
   "ff" #'find-file
   "fx" #'reopen-killed-file
   "fX" #'reopen-killed-file-fancy
   "fl" #'consult-line
   "fg" #'consult-ripgrep
   "rs" #'replace-string
   "rr" #'replace-regexp
   )

  ;; Change shape and color of each state
  (setq evil-normal-state-cursor '(hollow "light blue")
        evil-insert-state-cursor '(bar "medium sea green")
        evil-visual-state-cursor '(box "orange"))
  )

;; Extensions with evil and others
(use-package evil-colllection
  :straight (:type git :host github :repo "emacs-evil/evil-collection")
  :config
  (evil-collection-init '(dired consult corfu))
  )

;; Integration of lispy with evil
(use-package lispyville
  :after evil lispy
  :init
  (general-add-hook '(emacs-lisp-mode-hook lisp-mode-hook) #'lispyville-mode)
  :config
  (lispyville-set-key-theme '(operators c-w additional)))


;; Vim-like state in lisp
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
  (lisp-mode . lispy-mode))


;;* Tree sitter grammar
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
        (java "https://github.com/tree-sitter/tree-sitter-java")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp" "v0.20.0")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (vue "https://github.com/ikatyang/tree-sitter-vue")
        (prisma "https://github.com/victorhqc/tree-sitter-prisma")
        (dart "https://github.com/UserNobody14/tree-sitter-dart")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(comment
 (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))

(use-package lsp
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-disabled-clients
        '(semgrep-ls emmet-ls))
  )

(use-package evil-lsp
  :ensure nil
  :config
  (general-define-key
   :states '(normal motion visual)
   :keymaps 'override
   :prefix "SPC"

   "l" 'lsp-command-map
   )
  )

