;; -*- lexical-binding: t; -*-

;; * Load before alpaca

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


;; * Elpaca boostrap

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

(require 'cl-lib)

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
  (tool-bar-mode -1)			       ; This is much easier
  (menu-bar-mode -1)			; than needing to change
  (scroll-bar-mode -1)			; this on every OS
  (setq byte-compile-warnings '(not obsolete)) ;; Cl warnings
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t)
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq create-lockfiles nil)
  (setq abbrev-suggest t)
  ;; I finally caught on. This is annoying when it tries to.
  (setq require-final-newline nil)
  (setq mode-require-final-newline nil)
  ;; In case I use display-line-numbers-mode, use the relative display way.
  ;; (setq display-line-numbers 'relative)
  ;; (setq default-fill-column 74)

  ;; To remind me that this thing is posible
  ;; I yet don't feel worthy of such power
  ;; (setq enable-recursive-minibuffers t)

  ;; Why is 60 the default? Not because is such a low number but because
  ;; Is not a power of 2
  (setq kill-ring-max 1024)

  ;; Emacs, why is this so LOW
  (setq mark-ring-max (* 1024 8))

  ;; It doesn't do any harm
  (setq line-move-visual t)

  (setq highlight-nonselected-windows t)
  (setq use-dialog-box nil)	       ; Text-based options are better
;;; Seems this is not supported anymore, whatever it did
  ;; (setq bidi-display-reordering nil)
  (setq bidi-inhibit-bpa t)
  (setq-default bidi-paragraph-direction 'left-to-right)

  ;; Why this varible even exists?
  ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2020-09/msg01922.html
  (setq delete-pair-blink-delay 0)
  (setq font-lock-verbse nil)
  (setq byte-compile-verbose nil)
  ;; Default one; Why? Because everyone uses it, making more sense when
  ;; reading another person's code
  (setq-default tab-width 8)
  (setq default-tab-width 8)
  (setq inhibit-compacting-font-caches t)
  (setq use-package-always-demand (daemonp))
  (setq mouse-yank-at-point t)
  (setq comp-async-report-warnings-errors nil)
  (with-eval-after-load 's
    (if (s-suffix? "laptop" (system-name))
        (setq default-input-method "japanese")
      (setq default-input-method "japanese-mozc"))
    )
  (setq-default tab-always-indent t)
  (setq-default whitespace-line-column 1000)
  (setq-default cursor-type '(bar . 2)) ;; Change cursor to a bar
  (setq-default cursor-in-non-selected-windows nil)
  (setq-default blink-cursor-blinks 0)	; Never stop to blink
  (setq-default frame-title-format "%b %& emacs")
  ;; (setq-default garbage-collection-messages t)
  (setq-default indent-tabs-mode nil)
  (setq scroll-error-top-bottom t)
  ;; (setq-default
  ;;  prettify-symbols-alist
  ;;  '(("#+BEGIN_SRC" . "Λ")
  ;;    ("#+END_SRC" . "Λ")
  ;;    ("#+begin_src" . "Λ")
  ;;    ("#+end_src" . "Λ")
  ;;    ("lambda" . "λ")))
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
  (setq auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "auto-save-list/") t)))
  (setq undo-tree-history-directory-alist '(("." . (concat user-emacs-directory "undo"))))
  ;; Emacs doing one of its shennanigans again, if you subscribe to renaming
  ;; then this little gremling will do some moving that will trick your
  ;; subscribers into thinking the file move there, when it is just emacs
  ;; trickery
  (setq backup-by-copying t)

  (savehist-mode)

  (setq savehist-additional-variables '(register-alist))
  (setq split-height-threshold 4
        split-width-threshold 40
        split-window-preferred-function 'split-window-really-sensibly)

  (setq-default show-trailing-whitespace t)

  (setq delete-old-versions -1)
  (setq version-control t)
  (setq vc-make-backup-files t)

  ;; (setq auto-save-default nil)
  (setq sentence-end-double-space nil)
  (setq sgml-quick-keys nil)
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
  ;; * Set the font
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:family "Fantasque Sans Mono" :foundry "outline" :slant normal :weight normal :height 130 :width normal))))
   '(olivetti-fringe ((t (:foreground "#353535" :background "#353535")))))

  ;; * Find reference

  (defun mymy-find-defnition-at-point-noop ()
    (interactive)
    (user-error "`mymy-find-definition-at-point' as not been set."))

  (defvar-local mymy-find-definition-at-point #'mymy-find-defnition-at-point-noop
    "Function that is run when searching for reference, they find what
is the thing at point by themselves")

  (defun mymy-find-definition-at-point ()
    (interactive)
    (funcall mymy-find-definition-at-point))

  ;; * Load path

  (add-to-list 'load-path (concat user-emacs-directory "lib/"))

  ;; * Lib Requeries
  (require 'functions.el)
  (require 'vars.el)

  (defcustom powerline-buffer-size-suffix t
    "Display the buffer size suffix."
    :group 'powerline
    :type 'boolean)

  (defun powerline-vc ()
    (when (and (buffer-file-name (current-buffer)) vc-mode)
      (format " %s%s"
              (char-to-string 57504)
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

  ;; * Modeline
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
              " "))
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



  ;; * Frame
  (add-to-list 'default-frame-alist
               '(internal-border-width . 20))

  (set-frame-parameter (selected-frame) 'internal-border-width 20)

  (add-to-list 'default-frame-alist
               '(alpha-background . 99))
  (custom-set-variables '(read-extended-command-predicate #'command-completion-default-include-p))

  ;; Courtesy of https://www.reddit.com/r/emacs/comments/t07e7e/comment/hy88bum/
  (defun doom-make-hashed-auto-save-file-name-a (fn)
    "Compress the auto-save file name so paths don't get too long."
    (let ((buffer-file-name
           (if (or (null buffer-file-name)
                   (find-file-name-handler buffer-file-name 'make-auto-save-file-name))
               buffer-file-name
             (sha1 buffer-file-name))))
      (funcall fn)))
  (advice-add #'make-auto-save-file-name :around #'doom-make-hashed-auto-save-file-name-a)

  (defun doom-make-hashed-backup-file-name-a (fn file)
    "A few places use the backup file name so paths don't get too long."
    (let ((alist backup-directory-alist)
          backup-directory)
      (while alist
        (let ((elt (car alist)))
          (if (string-match (car elt) file)
              (setq backup-directory (cdr elt) alist nil)
            (setq alist (cdr alist)))))
      (let ((file (funcall fn file)))
        (if (or (null backup-directory)
                (not (file-name-absolute-p backup-directory)))
            file
          (expand-file-name (sha1 (file-name-nondirectory file))
                            (file-name-directory file))))))
  (advice-add #'make-backup-file-name-1 :around #'doom-make-hashed-backup-file-name-a)
  )

(use-package elisp-slime-nav
  :ensure t
  :init
  (defun mymy-emacs-lisp-hook ()
    (setq mymy-find-definition-at-point
          #'elisp-slime-nav-find-elisp-thing-at-point)
    (elisp-slime-nav-mode))
  :hook
  (emacs-lisp-mode . mymy-emacs-lisp-hook))

(use-package aggressive-indent
  :ensure t
  :config
  (defconst mymy-c-keywords (list "if" "for" "foreach" "while"))
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (or (derived-mode-p 'c++-mode)
             (derived-mode-p 'csharp-ts-mode))
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|foreach\\|while\\)\\b\\)"
                             (thing-at-point 'line)))))

  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (or (derived-mode-p 'c++-mode)
             (derived-mode-p 'csharp-ts-mode))
         (string-match
          (concat "\\(" (s-join "\\|" mymy-c-keywords) "\\)")
          (or (save-excursion
                ;; Go back to the previous character
                (backward-char)
                (thing-at-point 'symbol))
              ""))))
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

;; Add configuration which relies on after-init-hook, emacs-startup-hook,
;; etc to elpaca-after-init-hook so it runs after Elpaca has activated all
;; queued packages. This includes loading of saved customizations. e.g.

(add-hook 'elpaca-after-init-hook
          (lambda () (load custom-file 'noerror)))


;; * Emacs maintaining functionality
;; Here goes library installations, packages that help maintaining emacs

;; Explicit patching of functions and variables.
(use-package el-patch
  :ensure t
  :demand t)

;; Bring a little bit of clojure and more
(use-package dash
  :ensure t
  :demand t
  :config (global-dash-fontify-mode))

;; Some string utitlities
(use-package s
  :ensure t)

;; The best menu library
(use-package transient
  :ensure t)

;;When installing a package used in the init file itself,
;;e.g. a package which adds a use-package key word,
;;use the :wait recipe keyword to block until that package is installed/configured.
;;For example:
(use-package general
  :ensure t
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
    ;; Date
    ;; "C-S-t"
    ;; "C-t"
    ;; "C-v"
    ;; "M-v"
    ;; "C-b"
    ;; "C-f"
    ;; "M-u"
    ;; "C-M-p"
    ;; "C-M-f"
    ;; "C-c C-b"
    ;; "C-d"
    ;; "M-f"
    )

  (general-define-key
   :keymaps 'emacs-lisp-mode-map
   ;; I'm tired of this
   "C-c C-e" nil
   )
  (general-define-key
   "C-x C-y" 'pp-macroexpand-last-sexp
   )
  )

(use-package idea-darkula-theme
  :ensure t
  :init
  (setq custom--inhibit-theme-enable nil)
  ;; (setq custom--inhibit-theme-enable 'apply-only-user)
  :config
  ;; (push (substitute-in-file-name "~/.emacs.d/idea-darkula-theme/") custom-theme-load-path)
  (load-theme 'idea-darkula t)
  (let ((class '((class color) (min-colors 89)))
        (code-inline '(:foreground "#ff9bff" :background "#262031"))
        (code-block '(:foreground "#ffff7f" :background "#252046"))
        (ol1 '(:height 1.3 :weight bold :overline "#5d5862" :foreground "#c7c3cb" :background "#322d37"))
        (ol2 '(:height 1.0 :weight bold :overline "#efcab2" :foreground "#efcab2" :background "#3d2a2d"))
        (ol3 '(:height 1.0 :weight bold :foreground "#ffaae3" :background "#332038"))
        (ol4 '(:height 1.0 :weight bold :slant normal :foreground "#1a9cff"))
        (ol5 '(:height 1.0 :weight bold :slant normal :foreground "#21da7a"))
        (ol6 '(:height 1.0 :weight bold :slant italic :foreground "#ff883d"))
        (ol7 '(:height 1.0 :weight bold :slant italic :foreground "#d451d9"))
        (ol8 '(:height 1.0 :weight bold :slant italic :foreground "#077ffa"))
        )
    (custom-theme-set-faces
     'idea-darkula
     ;; Doesn't work for some reason
     ;; It actually works, just that java applies it weirdly and anywhere else is normal
     ;; '(font-lock-type-face ((t (:foreground "#A8B5C3"))))
     ;; End of dosen't work for some reason
     `(org-code ((,class ,code-inline)))
     `(org-block ((,class ,code-block)))
     `(org-block-begin-line ((,class (:underline "#5d595f" :foreground "#aeaab2" :background "#221e34"))))
     `(org-block-end-line ((,class (:overline "#5d595f" :foreground "#aeaab2" :background "#221e34"))))
     `(org-level-1 ((,class ,ol1)))
     `(org-level-2 ((,class ,ol2)))
     `(org-level-3 ((,class ,ol3)))
     `(org-level-4 ((,class ,ol4)))
     `(org-level-5 ((,class ,ol5)))
     `(org-level-6 ((,class ,ol6)))
     `(org-level-7 ((,class ,ol7)))
     `(org-level-8 ((,class ,ol8)))))

  (enable-theme 'idea-darkula)

  (defface font-lock-operator-face
    '((t :foreground "#8fff9e")) "Basic face for operator."
    :group 'basic-faces)

  (defface font-lock-number-face
    '((t :foreground "#6897BB")) "Basic face for number."
    :group 'basic-faces)
  )
;; Sync shell env variables to emacs env variables
(use-package exec-path-from-shell
  :ensure t
  :config
  (add-to-list 'exec-path-from-shell-variables "ANDROID_HOME")
  (add-to-list 'exec-path-from-shell-variables "ANDROID_SDK_ROOT")
  (when (or (memq window-system '(mac ns x))
            (daemonp))
    (exec-path-from-shell-initialize)))

;; * 1# Wait
;; All previous packages are somehow used later on and do not depend on
;; each other at the package dependency level.
(elpaca-wait)

;; * EVIL
;; Expands to: (elpaca evil (use-package evil :demand t))
;; Make emacs vim
(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-search-module 'evil-search)
  :config
  (evil-mode)
  (gsetq evil-undo-system 'undo-redo)
  (general-def 'normal emacs-lisp-mode-map
    "K" 'elisp-slime-nav-describe-elisp-thing-at-point)

  (general-override-mode 1)
  ;; (define-key evil-emacs-state-map (kbd "S-SPC") 'evil-normal-state)

  ;; It get's kind of annoying, maybe will activate later.
  (gsetq evil-want-empty-ex-last-command nil)

  (defvar mymy-buffer-map
    (-doto (make-sparse-keymap)
      (define-key (kbd "b") #'switch-to-last-buffer)
      (define-key (kbd "d") #'kill-current-buffer)
      ))

  (defvar mymy-find-map
    (-doto (make-sparse-keymap)
      (define-key (kbd "b") #'consult-buffer)
      (define-key (kbd "f") #'find-file)
      (define-key (kbd "f") #'find-file)
      (define-key (kbd "x") #'reopen-killed-file)
      (define-key (kbd "X") #'reopen-killed-file-fancy)
      (define-key (kbd "l") #'consult-line)
      (define-key (kbd "g") #'mymy-consult-grep-change-depending-on-arg)
      (define-key (kbd "s") #'describe-symbol)
      (define-key (kbd "d") #'mymy-find-definition-at-point)
      ))

  (defvar mymy-flycheck-map
    (-doto (make-sparse-keymap)
      (define-key (kbd "c") #'flycheck-buffer)
      (define-key (kbd "e") #'flycheck-explain-error-at-point)
      (define-key (kbd "l") #'flycheck-list-errors)
      (define-key (kbd "x") #'flycheck-disable-checker)
      (define-key (kbd "m") #'flycheck-mode)
      ))

  (defvar mymy-replace-map
    (-doto (make-sparse-keymap)
      (define-key (kbd "s") #'replace-string)
      (define-key (kbd "r") #'replace-regexp)
      ))

  (general-define-key
   :states '(normal motion visual)
   :keymaps 'override
   :prefix "SPC"

   "n" #'make-frame-command
   "b" mymy-buffer-map
   "f" mymy-find-map
   "!" mymy-flycheck-map
   "r" mymy-replace-map
   "a" #'org-agenda
   "g" #'magit
   )

  ;; Change shape and color of each state
  (setq evil-normal-state-cursor '(hollow)
        evil-insert-state-cursor '(bar)
        evil-visual-state-cursor '(box))
  )

;; Extensions with evil and others
(use-package evil-collection
  :after evil
  :ensure t
  ;; :ensure (evil-collection :host github :repo "emacs-evil/evil-collection")
  :config
  (evil-collection-init '( dired consult corfu
                           elisp-slime-nav elisp-mode
                           debug
                           magit magit-section magit-repos
                           magit-todos
                           org
                           vertico
                           wgrep wdired
                           flycheck
                           ))
  )

;; Integration of lispy with evil
(use-package lispyville
  :ensure t
  :after evil lispy
  :init
  (general-add-hook '(emacs-lisp-mode-hook lisp-mode-hook) #'lispyville-mode)
  :config
  (lispyville-set-key-theme '(operators c-w additional)))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))


;; Vim-like state in lisp
(use-package lispy
  :ensure t
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


;; * Tree sitter grammar
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

(when (version< "29" emacs-version)
  (use-package treesit-auto
    :ensure t
    :custom
    (treesit-auto-install 'prompt)
    :config
    (treesit-auto-add-to-auto-mode-alist 'all)
    (global-treesit-auto-mode)))

;; * Which key
(use-package which-key
  :ensure t
  :init
  (setq which-key-enable-extended-define-key t
        which-key-side-window-location 'bottom
        which-key-window-max-height 0.4
        which-key-window-max-width 0.66
        )
  :config
  (which-key-mode)
  )

;; * Lsp mode
(use-package lsp-mode
  :ensure t
  :init
  (defun mymy-lsp-mode-hook ()
    (setq mymy-find-definition-at-point #'lsp-find-definition))

  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-disabled-clients
        '(semgrep-ls emmet-ls))
  (setq lsp-auto-execute-action nil)
  :config
  (define-key lsp-mode-map (kbd "M-?") #'lsp-find-references)
  (define-key lsp-mode-map (kbd "M-/") #'lsp-find-implementation)
  (define-key lsp-mode-map (kbd "M-.") #'lsp-find-definition)
  (define-key lsp-mode-map (kbd "C-;") #'lsp-iedit-highlights)
  (define-key lsp-mode-map (kbd "C-M-;") #'lsp-iedit-highlights)
  :hook (lsp-mode . mymy-lsp-mode-hook)
  )

(use-package evil-lsp
  :after evil lsp-mode
  :ensure nil
  :no-require t
  :config
  (general-define-key
   :states '(normal motion visual)
   :keymaps 'lsp-mode-map
   :prefix "SPC"

   "l" lsp-command-map))

(use-package lsp-omnisharp
  :ensure nil
  :no-require t
  :config

  (setenv "DOTNET_RUNTIME_ID" "linux-x64")
  :hook (csharp-ts-mode . lsp)
  )

(use-package lsp-java
  :ensure t
  :config
  (add-hook 'java-mode-hook 'lsp)
  (add-hook 'java-ts-mode-hook 'lsp)
  )

(use-package lsp-haskell
  :ensure t
  ;; :defer 5
  :after lsp-mode
  ;; Uncontable tales i have of how this monster have ruined my day, not
  ;; because of itself, but because Of how much ram it needs and how my
  ;; little School-gorverment-given computer hogs from the effort of
  ;; keeping this thing afloat
  :config
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp)
  (setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/.ghcup/bin/")))
  (setq lsp-haskell-server-path (expand-file-name "~/.ghcup/bin/haskell-language-server-wrapper"))
  )

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :init
  (setq dap-netcore-install-dir "/usr/bin/netcoredbg")
  (require 'dap-ui)
  :config

  ;; (setq dap-print-io nil)
  ;; (setq dap-print-io t)
  (require 'dap-python)

  (require 'dap-netcore)
  (require 'dap-php)
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))

  (defun mymy-dap-eval-dwim (arg)
    (interactive "P")
    (if (equal '(4) arg)
        (dap-eval)
      (if (region-active-p)
          (call-interactively #'dap-eval-region)
        (dap-eval-thing-at-point))))

  (general-define-key
   :keymaps 'csharp-ts-mode-map
   "C-x C-e" #'mymy-dap-eval-dwim
   )

  (setq dap-internal-terminal #'dap-internal-terminal-vterm)

  ;; TODO: Deal with default template, like dotnet.
  (dap-register-debug-template ".NET Core Launch (web)"
                               (list :type "coreclr"
                                     :request "launch"
                                     :mode "launch"
                                     :dap-compilation "dotnet build"
                                     :stopAtEntry :json-false
                                     :serverReadyAction '(("action" . "openExternally")
                                                          ("pattern" . "\\bNow listening on:\\s+(https?://\\S+)"))
                                     :env '(("ASPNETCORE_ENVIRONMENT" . "Development")
                                            ("COMPLUS_ReadyToRun" . "0")
                                            )
                                     ))

  (setq dap-ui-expressions-expand-depth 1)

  (defun mymy-dap-netcore--populate-args (conf)
    "Populate CONF with arguments to launch or attach netcoredbg."
    (dap--put-if-absent conf :dap-server-path (list (dap-netcore--debugger-locate-or-install) "--interpreter=vscode"))
    (pcase (plist-get conf :mode)
      ("launch"
       (dap-netcore--populate-args conf))
      ("attach"
       (dap--put-if-absent conf :processId
                           (string-to-number
                            (plist-get
                             (mymy-select-process
                              ;; (lambda (it)
                              ;;   (s-contains? "dotnet" (plist-get it :command)))
                              )
                             :pid))))))

  (dap-register-debug-provider
   "coreclr"
   'mymy-dap-netcore--populate-args)

  (dap-register-debug-template ".NET Core Attach (web)"
                               (list :type "coreclr"
                                     :request "attach"
                                     :mode "attach"
                                     :env '(("COMPLUS_ReadyToRun" . "0"))
                                     ))

  ;; If I run dotnet test with env VSTEST_HOST_DEBUG=1 I can attach a
  ;; debugger and debug a test

  )
;; * Haskell


(use-package haskell-mode
  ;; :ensure (haskell-mode :host github :type git :repo "haskell/haskell-mode")
  :ensure t
  :config
  (define-key haskell-mode-map [f8] 'haskell-navigate-imports)
  (custom-set-variables '(haskell-process-type 'cabal-repl))
  )
;; * Magit

(use-package magit
  :ensure t
  )

(use-package forge
  :after magit
  :ensure t)

;; * Consult
(use-package consult
  :ensure t
  :config
  (setq consult-fontify-max-size 1024)
  :config
  (general-define-key
   :keymaps 'vertico-map
   "C-l" 'up-directory)

  ;; Yeah, super redundant, that is the point.
  (defun mymy-consult-grep-change-depending-on-arg (arg &optional dir initial)
    "Change consult grep depending on arg"
    (interactive "P")
    (cond
     ((equal arg '(4))
      (consult-grep dir initial))
     (t
      (consult-ripgrep initial))))

  :config
  (recentf-mode 1)
  ;; With this package I can embark-export consult-line candidates and
  ;; then edit with occur-edit-mode (e). Also, the export buffer
  ;; becomes a occur buffer.
  (general-define-key
   "M-x" 'execute-extended-command
   "C-x b" 'consult-buffer
   "C-x C-f" 'find-file
   "C-c h s" 'consult-line)

  ;; Much better than openning a window
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; * Projectile
(use-package projectile
  :demand t
  :ensure t
  ;; TODO: Come here later
  :init

  (defun mymy-projectile-root-csharp (dir)
    "Retrieve the root directory of a C# project in DIR.
This function gives priority to .sln files over .csproj files."
    (let ((root (or (projectile-locate-dominating-file dir "*.sln")
                    (projectile-locate-dominating-file dir "*.csproj"))))
      (and root (expand-file-name root))))


  ;; This is my fault, but some projects are just not git repositories so I
  ;; have to do some preprocesing in emacs to compensate.
  (gsetq projectile-indexing-method
         'hybrid)
  ;; (general-define-key
  ;;  :keymaps 'projectile-mode-map
  ;;  "C-c t" 'projectile-command-map)
  ;; Defaults to 1

  (setq projectile-completion-system 'auto)
  (setq projectile-enable-caching t)
  :config
  (setq projectile-project-root-functions
        '(projectile-root-local
          projectile-root-marked
          mymy-projectile-root-csharp
          projectile-root-bottom-up
          projectile-root-top-down
          projectile-root-top-down-recurring))
  (add-to-list 'projectile-project-search-path '("~/Projects/" . 5))
  (add-to-list 'projectile-project-search-path mymy-organization-system-directory)
  (add-to-list 'projectile-project-search-path "~/.xmonad/")
  (add-to-list 'projectile-project-search-path `(,user-emacs-directory . 1))

  (add-to-list 'projectile-globally-ignored-directories
               "^node_modules$")
  ;; Will do things like this manually.
  (setq projectile-auto-discover nil)
  ;; Order is impportant
  ;; (add-to-list 'projectile-project-root-functions
  ;;              #'mymy-find-nearest-solution-file t)
  ;; (add-to-list 'projectile-project-root-functions
  ;;              #'not-mymy-find-nearest-chsarp-project t)
  ;; (add-to-list 'projectile-project-root-files
  ;;              "*.sln")

  :hook
  (elpaca-after-init . projectile-mode))

(use-package consult-projectile
  :after (consult projectile)
  :ensure t
  :init
  (setq consult-projectile-sources
        '(consult-projectile--source-projectile-buffer
          consult-projectile--source-projectile-file
          consult-projectile--source-projectile-dir))

  (defvar mymy-projectile-map
    (-doto (make-sparse-keymap)
      (define-key (kbd "p") #'consult-projectile)
      (define-key (kbd "s") #'consult-projectile-switch-project)
      (define-key (kbd "S") #'projectile-save-project-buffers)
      (define-key (kbd "fb") #'consult-projectile-switch-to-buffer)
      (define-key (kbd "fd") #'consult-projectile-find-dir)
      (define-key (kbd "ff") #'consult-projectile-find-file)
      (define-key (kbd "d") #'consult-projectile-find-dir)
      (define-key (kbd "D") #'projectile-dired)
      (define-key (kbd "i") #'projectile-invalidate-cache)
      (define-key (kbd "k") #'projectile-kill-buffers)
      (define-key (kbd "r") #'projectile-replace)
      (define-key (kbd "R") #'projectile-replace-regexp)
      (define-key (kbd "g") #'consult-ripgrep)
      ))

  (general-define-key
   :states '(normal motion visual)
   :keymaps 'override
   :prefix "SPC"
   "p" mymy-projectile-map
   )
  )

;; * Vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  (setq enable-recursive-minibuffers t)
  )

;; * Marginalia
(use-package marginalia
  :ensure t
  :config
  ;; Until I find the way.
;;; I don't remember why I said the previous thing
  (marginalia-mode)
  )

;; * Embark
(use-package embark
  ;; Unnecessary? Maybe, but this thing wasn't loading symlinking every .el file so I had to put it myself.
  ;; :straight (:files ("*.el"))
  :ensure t
  :config
  (general-define-key
   "C-," 'embark-act
   "M-," 'embark-dwim
   "C-c i" 'embark-act
   )

  ;; (setq display-buffer-alist
  ;;       '())

  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 (window-parameters (mode-line-format . none))))

  (setq embark-verbose-indicator-display-action '(display-buffer-in-direction
                                                  (direction . top)))

  (defun mymy-act-at-point (arg)
    "Depending on the context, apply a function that acts around the point, by default this "
    (interactive "P")
    (let ((default-action 'embark-act))
      (cond
       (arg (funcall default-action))
       ;; ((eglot-current-server) (call-interactively #'eglot-code-actions))
       ((progn (require 'lsp-mode) lsp-mode) (call-interactively #'lsp-execute-code-action))
       (t (funcall default-action)))))

  (general-define-key
   :keymaps 'override
   "M-<return>" 'mymy-act-at-point)
  ;; (setq embark-verbose-indicator-display-action '(display-buffer-reuse-window))
  )

(use-package embark-consult
  ;; :no-require t
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  )

(use-package wgrep
  :ensure t)

;; * Orderless
(use-package orderless
  :ensure t
  :config
  ;; ;; Put orderless at last since orderless put me things almost at random.
  ;; ;; (add-to-list 'completion-styles 'orderless t)
  ;; ;; (setq completion-styles '(basic partial-completion orderless))
  (setq completion-styles '(basic partial-completion substring orderless))
  ;; ;; matching characters in order, but non-consecutively
  ;; ;; (add-to-list 'orderless-matching-styles 'orderless-flex t)
  ;; (setq orderless-matching-styles '(;; orderless-literal
  ;;                                   orderless-regexp orderless-prefixes))
  (setq orderless-matching-styles '(orderless-literal orderless-regexp ;; orderless-flex
                                                      ))
  (setq completion-category-overrides '((file (styles basic partial-completion substring)))))

;; * Smartparens
(use-package smartparens
  :ensure t
  :init
  ;; (defun mymy/smartparens-hook ()
  ;;   (smartparens-global-mode)
  ;;   (show-smartparens-global-mode))
  (setq mymy-excluded-apostrophe-modes
        '(emacs-lisp-mode clojure-mode cider-mode slime-mode lisp-mode
                          haskell-mode))
  :config
  (sp-local-pair mymy-excluded-apostrophe-modes "'" "'" :actions nil)
  (general-define-key
   :keymaps 'smartparens-mode-map
   "M-(" 'sp-wrap-round)
  :hook (prog-mode . smartparens-mode))

(use-package smartparens-haskell
  :after smartparens haskell-mode
  :ensure nil
  :no-require t
  :hook (haskell-mode . (lambda () (require 'smartparens-haskell))))

;; * Corfu


(use-package corfu
  ;; Explictly tell that we want all the files in extensions (not
  ;; necessary, but don't want be manually dealing with this) since this is
  ;; only building corfu.el
  ;; :straight (corfu :files ("*.el" "extensions/*.el"))
  :ensure t
  ;; Optional customizations
  :config
  (el-patch-defun corfu--filter-completions (&rest args)
    "Compute all completions for ARGS with lazy highlighting."
    (dlet ((completion-lazy-hilit t) (completion-lazy-hilit-fn nil))
      (el-patch-wrap 2 0
        (if (eval-when-compile (>= emacs-major-version 30))
            (el-patch-splice 2 0
              (static-if (>= emacs-major-version 30)
                  (cons (apply #'completion-all-completions args) completion-lazy-hilit-fn)
                (cl-letf* ((orig-pcm (symbol-function #'completion-pcm--hilit-commonality))
                           (orig-flex (symbol-function #'completion-flex-all-completions))
                           ((symbol-function #'completion-flex-all-completions)
                            (lambda (&rest args)
                              ;; Unfortunately for flex we have to undo the lazy highlighting, since flex uses
                              ;; the completion-score for sorting, which is applied during highlighting.
                              (cl-letf (((symbol-function #'completion-pcm--hilit-commonality) orig-pcm))
                                (apply orig-flex args))))
                           ((symbol-function #'completion-pcm--hilit-commonality)
                            (lambda (pattern cands)
                              (setq completion-lazy-hilit-fn
                                    (lambda (x)
                                      ;; `completion-pcm--hilit-commonality' sometimes throws an internal error
                                      ;; for example when entering "/sudo:://u".
                                      (condition-case nil
                                          (car (completion-pcm--hilit-commonality pattern (list x)))
                                        (t x))))
                              cands))
                           ((symbol-function #'completion-hilit-commonality)
                            (lambda (cands prefix &optional base)
                              (setq completion-lazy-hilit-fn
                                    (lambda (x) (car (completion-hilit-commonality (list x) prefix base))))
                              (and cands (nconc cands base)))))
                  (cons (apply #'completion-all-completions args) completion-lazy-hilit-fn))))))))

  (el-patch-defun corfu--delete-dups (list)
    "Delete `equal-including-properties' consecutive duplicates from LIST."
    (let ((beg list))
      (while (cdr beg)
        (let ((end (cdr beg)))
          (while (equal (car beg) (car end)) (pop end))
          ;; The deduplication is quadratic in the number of duplicates.  We can
          ;; avoid the quadratic complexity with a hash table which takes
          ;; properties into account (available since Emacs 28).
          (while (not (eq beg end))
            (let ((dup beg))
              (while (not (eq (cdr dup) end))
                ;; bug#6581: `equal-including-properties' uses `eq' to compare
                ;; properties until 29.1.  Approximate by comparing
                ;; `text-properties-at' position 0.
                (el-patch-swap
                  (if (static-if (< emacs-major-version 29)
                          (equal (text-properties-at 0 (car beg))
                                 (text-properties-at 0 (cadr dup)))
                        (equal-including-properties (car beg) (cadr dup)))
                      (setcdr dup (cddr dup))
                    (pop dup))
                  (if (if (eval-when-compile (< emacs-major-version 29))
                          (equal (text-properties-at 0 (car beg))
                                 (text-properties-at 0 (cadr dup)))
                        (equal-including-properties (car beg) (cadr dup)))
                      (setcdr dup (cddr dup))
                    (pop dup))
                  )))
            (pop beg)))))
    list)

  (gsetq corfu-cycle t) ;; Enable cycling for `corfu-next/previous'
  (gsetq corfu-auto t)  ;; Enable auto completion
  (gsetq corfu-separator ?\s) ;; Orderless field separator
  ;; (gsetq corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (gsetq corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (gsetq corfu-preview-current nil) ;; Disable current candidate preview
  ;; (gsetq corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (gsetq corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (gsetq corfu-scroll-margin 0) ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.

  ;; Set the first delay and consecutive delay for help on current
  ;; element
  (gsetq corfu-echo-delay (cons 1.5 0.8))

  (comment
   ;; https://github.com/minad/corfu?tab=readme-ov-file#debugging-corfu
   ;; "When you observe an error in the corfu--post-command post
   ;; command hook"

   (setq debug-on-error t)

   (defun force-debug (func &rest args)
     (condition-case e
         (apply func args)
       ((debug error) (signal (car e) (cdr e)))))

   (advice-add #'corfu--post-command :around #'force-debug)
   )

  :init
  (general-define-key
   "C-M-e" 'completion-at-point
   "M-/" 'dabbrev-completion
   "C-M-/" ' dabbrev-expand
   )

  (general-define-key
   :keymap 'corfu-map
   ;; Default: M-h
   "M-h" 'corfu-info-documentation
   ;; Default: M-g
   "M-g" 'corfu-info-location
   ;; Default: completion-at-point, TAB
   [completion-at-point] 'corfu-complete
   "TAB" 'corfu-complete)

  (global-corfu-mode)
  (corfu-echo-mode)
;;; Like company quickhelp, except that opens the frame
  ;; (corfu-popupinfo-mode -1)
  )

(use-package cape
  :ensure t
  :after corfu
  :config
  (general-define-key
   :keymap 'org-mode-map
   "C-M-k" 'cape-file)
  )

(use-package nerd-icons-corfu
  ;; :straight (:host github :type git :repo "LuigiPiucco/nerd-icons-corfu")
  :ensure t
  :after corfu nerd-icons
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; * Nerd icons

(use-package nerd-icons
  :ensure t)

;; * Terminal here

;; Love this, just a open-system terminal here
(use-package terminal-here
  :ensure t
  :config
  (global-set-key (kbd "C-<f5>") #'terminal-here-launch)
  (global-set-key (kbd "C-<f6>") #'terminal-here-project-launch)
  (setq terminal-here-linux-terminal-command '("kitty" "--single-instance"))
  (setq terminal-here-command-flag "--")
  ;; (when (executable-find "poetry")
  ;;   (global-set-key (kbd "C-<f3>") (lambda () (interactive) (terminal-here-launch (list (executable-find "poetry") "shell")))))

  (defun mymy-set-xmonad-project-dir-here ()
    (interactive)
    (and (y-or-n-p "Want to change the xmonad directory?")
         (when (not (= 0 (shell-command (format (expand-file-name "~/Scripts/xmonadctl -a XMONAD_CHANGE_DIR %S") default-directory))))
           (error "Error setting the xmonad dir")))
    )
  (global-set-key (kbd "C-<f4>") #'mymy-set-xmonad-project-dir-here)
  )

;; * Undo tree

(use-package undo-tree
  :ensure t
  :bind
  (:map undo-tree-visualizer-mode-map
        ("q" . undo-tree-visualizer-quit)
        ("N" . undo-tree-visualize-switch-branch-left)
        ("n" . undo-tree-visualize-redo)
        ("u" . undo-tree-visualize-undo)
        ("U" . undo-tree-visualize-switch-branch-right)))

;; * Dashboard
(use-package dashboard
  :ensure t
  :init
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-footer nil)
  (setq dashboard-show-shortcuts t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t)
  ;; Projects, Areas of activity, Reading list
  (setq dashboard-items '((bookmarks . 5)
                          (projects . 5)))
  :config
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (dashboard-setup-startup-hook))

(use-package all-the-icons
  :ensure t)

(use-package page-break-lines
  :ensure t)

(use-package doom-dashboard
  ;; Use my repo for the moment
  :ensure (doom-dashboard :host github
                          :repo "cheerio-pixel/doom-dashboard"
                          :files ("*.el" "banners/*.txt"))
  :after (dashboard all-the-icons)
  :demand t
  ;; Movement keys like doom.
  :bind
  (:map dashboard-mode-map
        ("<remap> <dashboard-previous-line>" . widget-backward)
        ("<remap> <dashboard-next-line>" . widget-forward)
        ("<remap> <previous-line>" . widget-backward)
        ("<remap> <next-line>"  . widget-forward)
        ("<remap> <right-char>" . widget-forward)
        ("<remap> <left-char>"  . widget-backward))
  :config
  (gsetq dashboard-banner-logo-title "E M A C S")
  (gsetq dashboard-startup-banner
         (concat (file-name-directory (locate-library "doom-dashboard")) "bcc.txt")) ; Use banner you want
  (gsetq dashboard-footer-icon
         (nerd-icons-faicon "nf-fa-github_alt" :face 'success :height 1.5))
  (gsetq dashboard-page-separator "\n")
  (gsetq dashboard-startupify-list `(dashboard-insert-banner
                                     dashboard-insert-banner-title
                                     dashboard-insert-newline
                                     dashboard-insert-items
                                     ,(dashboard-insert-newline 2)
                                     dashboard-insert-init-info
                                     ,(dashboard-insert-newline 2)
                                     doom-dashboard-insert-homepage-footer))
  (gsetq doom-dashboard-shortmenu-functions
         `((recents   . recentf)
           (bookmarks . bookmark-jump)
           (projects  . consult-projectile-switch-project)
           (agenda    . org-agenda)))
  (gsetq dashboard-item-generators
         '((recents   . doom-dashboard-insert-recents-shortmenu)
           (bookmarks . doom-dashboard-insert-bookmark-shortmenu)
           (projects  . doom-dashboard-insert-project-shortmenu)
           (agenda    . doom-dashboard-insert-org-agenda-shortmenu)))
  (gsetq dashboard-items '(projects agenda bookmarks recents)))

;; * Dirvish
(use-package dirvish
;;; Why did I disable this?
  ;; :disabled
  :ensure t
  :init
  (dirvish-override-dired-mode)
  (setq dirvish-preview-dispatchers (remove 'archive dirvish-preview-dispatchers))
  ;; (setq dirvish-attributes '(file-size hl-line all-the-icons))
  (setq dirvish-attributes
        '(vc-state subtree-state all-the-icons collapse git-msg file-time file-size))
  (setq dired-listing-switches "-al --group-directories-first")
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  :config
  ;; (setq dirvish-attributes nil)
  (defhydra hydra-dirvish (:hint nil)
    ;; TODO: Finish this
    ""
    ("a"   dirvish-quick-access)
    ("f"   dirvish-file-info-menu)
    ("y"   dirvish-yank-menu)
    ("N"   dirvish-narrow)
    ("^"   dirvish-history-last)
    ("h"   dirvish-history-jump)        ; remapped `)describe-mode'
    ("s"   dirvish-quicksort)   ; remapped `dired-sort-toggle-or-edit'
    ("v"   dirvish-vc-menu)     ; remapped `dired-view-file'
    ("TAB" dirvish-subtree-toggle)
    ("M-f" dirvish-history-go-forward)
    ("M-b" dirvish-history-go-backward)
    ("M-l" dirvish-ls-switches-menu)
    ("M-m" dirvish-mark-menu)
    ("M-t" dirvish-layout-toggle)
    ("M-s" dirvish-setup-menu)
    ("M-e" dirvish-emerge-menu)
    ("M-j" dirvish-fd-jump)
    ("."   nil)
    )
  :bind
  ((:map dirvish-mode-map
         ("a"   . dirvish-quick-access)
         ("f"   . dirvish-file-info-menu)
         ("y"   . dirvish-yank-menu)
         ("N"   . dirvish-narrow)
         ("^"   . dirvish-history-last)
         ("h"   . dirvish-history-jump) ; remapped `describe-mode'
         ("s"   . dirvish-quicksort) ; remapped `dired-sort-toggle-or-edit'
         ("v"   . dirvish-vc-menu)   ; remapped `dired-view-file'
         ("TAB" . dirvish-subtree-toggle)
         ("M-f" . dirvish-history-go-forward)
         ("M-b" . dirvish-history-go-backward)
         ("M-l" . dirvish-ls-switches-menu)
         ("M-m" . dirvish-mark-menu)
         ("M-t" . dirvish-layout-toggle)
         ("M-s" . dirvish-setup-menu)
         ("M-e" . dirvish-emerge-menu)
         ("M-j" . dirvish-fd-jump)
         )
   (:map dired-mode-map (("C-l" . dired-up-directory)
                         ("." . hydra-dirvish/body ;; dirvish-dispatch
                          )))))


;; * Tempel

(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")
  :ensure t

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("C-c e" . tempel-complete)
         ("M-*" . tempel-insert))

  :init
  (gsetq tempel-trigger-prefix "<")
  (setq mymy-template-files (expand-file-name
                             "tempel/"
                             dropbox-dir))
  (setq tempel-path (list (concat mymy-template-files "*.eld")
                          (concat mymy-template-files "*/*.eld")))

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  :config

  ;; From documentation
  (defun tempel-include (elt)
    "Introduces the element 'i', it includes other templates by name."
    (when (eq (car-safe elt) 'i)
      (if-let (template (alist-get (cadr elt) (tempel--templates)))
          (cons 'l template)
        (message "Template %s not found" (cadr elt))
        nil)))

  ;; Work in Progress
  ;; Example of syntax
  (comment
   (b src
      ((lang "arsdasrt")
       )
      )
   )
  (defun mymy-tempel-bind (elt)
    "Introduces the element 'b', it includes other templates by name
and also binds their names fields with lisp expressions (or values).
It is essentially the element include but with args."
    (when (eq (car-safe elt) 'b)
      (if-let (template (alist-get (cadr elt) (tempel--templates)))
          (cons 'l (dolist (caddr elt) template))
        (message "Template %s not found" (cadr elt))
        nil))
    )

  (add-to-list 'tempel-user-elements #'tempel-include)

  (defun mymy-tempel-add-org-babel-languages ()
    "Load all snippets to tempel"
    (when (string-equal major-mode "org-mode")
      (->> org-babel-load-languages
           (-filter 'cdr)
           (-map 'car)
           (--map `(,(intern-soft it)
                    ,(concat "#+begin_src " (symbol-name it))
                    n q n r n> "#+end_src" )))))

  ;; This thing is used in template
  (add-to-list 'tempel-template-sources #'mymy-tempel-add-org-babel-languages))

;; * Org mode

(use-package doct
  ;; Description: doct is a function that provides an alternative,
  ;; declarative syntax for describing Org capture templates.
  :ensure t
  )

(use-package org-contrib
  :ensure t)


(use-package org
  :after doct org-contrib
  :init
  (gsetq org-file-apps
         '((auto-mode . emacs)
           (directory (direction . top)
                      . emacs)
           ("\\.mm\\'" . default)
           ("\\.x?html?\\'" . default)
           ("\\.pdf\\'" . "zathura %s")))

  (gsetq org-directory mymy-organization-system-directory)

  (setq mymy-org-gtd-file "2023-12-23_GTD.org")
  (setq mymy-org-inbox-file "2023-12-26_inbox.org")
  (setq mymy-org-projects-file "2024-01-05_projects.org")
  (setq mymy-org-done-file "2024-01-09_done.org")
  (setq mymy-org-school-file "20240117T132013--school.org")
  (gsetq org-agenda-files (list mymy-org-gtd-file
                                mymy-org-inbox-file
                                mymy-org-projects-file
                                mymy-org-school-file
                                "mobile/2024-01-01_mobile_inbox.org"))

  (gsetq org-refile-targets
         '((nil :maxlevel . 3)
           (mymy-org-done-file :maxlevel . 1)
           (mymy-org-school-file :maxlevel . 1)
           (org-agenda-files :maxlevel . 1)))

  ;; Default: t
  ;; Count checkbox statistics only for direct children of heading.
  (gsetq org-checkbox-hierarchical-statistics nil)

  ;; Other option, attached to add attachments
  ;; This one adds a link to the file into the store upon attaching
  (setq org-attach-store-link-p 'file)

  ;; Make Agenda commands appear on top
  (add-to-list 'display-buffer-alist
               '(" \\*Agenda Commands\\*"
                 ;; Okay, I'm as confused as the documentation.
                 (display-buffer-in-direction display-buffer-pop-up-window)
                 (window-parameters (mode-line-format . none))))

  ;;** Org Capture Templates
  (gsetq org-capture-templates
         ;; https://orgmode.org/manual/Template-expansion.html#Template-expansion
         ;; https://orgmode.org/manual/Template-elements.html#Template-elements
         (doct
          `(("GTD" :keys "t"
             :file ,mymy-org-inbox-file
             :headline "Tasks"
             :template ("* TODO %?"
                        ":PROPERTIES:"
                        ":CREATED: %<%Y-%m-%d-%H-%M-%S>"
                        ":END:"
                        ""
                        "- Project "
                        "- What to do"
                        "  "))
            ("Documentation" :keys "d"
             :type entry
             :file ,mymy-org-inbox-file
             :headline "Tasks"
             :template ("* TODO %?"
                        ":PROPERTIES:"
                        ":CREATED: %<%Y-%m-%d-%H-%M-%S>"
                        ":END:"
                        ""
                        "- Documentation"
                        ""
                        "  USING SPEC 0.1.0"
                        "  USING program@ver"
                        ""
                        "  ACTION ")
             :children (("Standard" :keys "d")))
            ("Homework" :keys "h"
             :type entry
             :file ,mymy-org-school-file
             :headline "Inbox"
             :template ("* TODO %? [/]"
                        ":PROPERTIES:"
                        ":CREATED: %<%Y-%m-%d-%H-%M-%S>"
                        ":END:"
                        ""
                        "DEADLINE: %^{Deadline}T"
                        "SCHEDULED: %^{Scheduled}t"
                        ""
                        "- Tasks"
                        "  - [ ] "
                        ""
                        "- Assignment"
                        ""
                        "  "
                        )
             )
            ("Note" :keys "n"
             :type entry
             :file ,mymy-org-inbox-file
             :headline "Notes"
             :template ("* %? :notes:"
                        ":PROPERTIES:"
                        ":CREATED: %<%Y-%m-%d-%H-%M-%S>"
                        ":END:"
                        ""
                        "- Elements: Content, Concept, Composition. Main idea, Examples, Related")))))

  ;; Org define keys (:prefix C-c o)
  (general-define-key
   :prefix "C-c o"
   "c" 'org-capture
   "w" 'hydra-org-web-tools/body
   "a" 'org-agenda
   )

  ;; Set indentation level one to one with src declaration
  (setq org-edit-src-content-indentation 0)

  ;; Timestmap Org ID
  ;; Use timmestamps instead of UUID
  (gsetq org-id-method 'ts)
  ;; Default: "%Y%m%dT%H%M%S.%6N"
  (gsetq org-id-ts-format "%Y%m%dT%H%M%S.%9N%Z%z")

  ;; Org attach
  (require 'org-attach)
  (gsetq org-attach-id-to-path-function-list
         '(org-attach-id-ts-folder-format org-attach-id-uuid-folder-format))
  (gsetq org-attach-directory mymy-organization-system-directory-attachments)

  (setq org-fontify-whole-heading-line t)

  (setq org-clock-string-limit 25)

  (el-patch-defcustom org-mark-ring-length 4
    "Number of different positions to be recorded in the ring.
Changing this requires a restart of Emacs to work correctly."
    ;; Why they don't do this and warn that this will reset your mark ring?
    (el-patch-add
      :set (lambda (var val)
             (set var val)
             (setq org-mark-ring nil)
             (setq org-mark-ring-last-goto nil) ;in case file is reloaded

             (dotimes (_ org-mark-ring-length) (push (make-marker) org-mark-ring))
             (setcdr (nthcdr (1- org-mark-ring-length) org-mark-ring)
                     org-mark-ring)
             ))
    :group 'org-link-follow
    :type 'integer)

  ;; I mean, this is useful once you start going
  (setq org-mark-ring-length 100)

  (defun mymy-configure-org-tags ()
    "Do some tweaking on the aesthetics of org-tags"
    (progn
      (set-face-attribute
       'org-tag nil
       :background "purple"
       :foreground "white")
      ;; Hide colons in tags of org mode heading
      (font-lock-add-keywords 'org-mode
                              ;; Match tree things,
                              ;; group 0 the heading and tags
                              ;; group 1 the colons outside with everything in between
                              ;; group 2 the inside of the outised colons
                              '(("^\\*+ \\(?:.*[ 	]\\)?\\(:\\([[:alnum:]_@#%:]+\\):\\)[ 	]*$"
                                 ;; The previous regex was the ancher, this is the real hightlight
                                 ":"
                                 ;; Skip the heading. Go to the beginning of tags. Which is group 1
                                 ;; If you return a point, this will become
                                 ;; the limit. So moving and return a point
                                 ;; (using save excursion) will enclose a
                                 ;; region in which to apply the regex
                                 (goto-char (match-beginning 1))
                                 nil
                                 ;; After matching all : at position declared in pre-form.
                                 ;; Put facade (no better word, since it's so bad) to colons
                                 (0 (put-text-property (match-beginning 0) (match-end 0) 'display " ")))))
      (setq org-tags-column 0)
      (setq org-tag-faces
            '(("Hold" (:foreground "yellow" :weight bold))
              ("Kobo" (:foreground "red" :weight bold))))))

  (mymy-configure-org-tags)

  (defun mymy-org-link-activate-link (start end _path bracketp)
    "Make the link [[PATH][DESC]] show like [[DESC]]"
    (when bracketp
      (let ((visible-start (or (match-beginning 3) (match-beginning 2)))
            (visible-end (or (match-end 3) (match-end 2))))
        (remove-text-properties start (1+ start) '(invisible nil))
        (remove-text-properties (1- visible-start) visible-start '(invisible nil))
        (remove-text-properties visible-end end '(invisible nil)))))

  (org-link-set-parameters "id" :activate-func #'mymy-org-link-activate-link)
  (org-link-set-parameters "cite" :activate-func #'mymy-org-link-activate-link)

  ;; Found this somewhere else, the core idea is not mine but the
  ;; other things are mine
  (defun org-count-subentries (&optional message pos match scope level)
    "Return number of subentries for entry at POS.
MATCH and SCOPE are the same as for `org-map-entries', but
SCOPE defaults to 'tree.
By default, all subentries are counted; restrict with LEVEL."
    (interactive t)
    (save-excursion
      (goto-char (or pos (point)))
      ;; If we are in the middle of an entry, use the current heading.
      (org-back-to-heading t)
      (let* ((maxlevel (when (and level (org-current-level))
                         (+ level (org-current-level))))
             (subentries (1- (length
                              (delq nil
                                    (org-map-entries
                                     (lambda ()
                                       ;; Return true, unless below maxlevel.
                                       (or (not maxlevel)
                                           (<= (org-current-level) maxlevel)))
                                     match (or scope 'tree)))))))
        (when message
          (message (concat (when match (concat match ": ")) "%s subentries") subentries))
        (when match
          (save-match-data
            (pcase match
              ((pred (lambda (n) (ignore-errors (= 0 (string-match org-todo-regexp n)))))
               (list (intern (concat ":" match)) subentries))
              ((pred (lambda (n) (ignore-errors (= 0 (string-match (s-wrap org-tag-re ":") n)))))
               (list (intern (string-trim-right match ":+")) subentries))
              ((pred keywordp)
               (list match subentries))
              ((pred stringp)
               (list (intern (concat ":" match)) subentries))
              ;; If match is t then return the subentries
              (_ subentries)))))))

  (setq spaceline-org-clock-format-function 'dwim/org-clock-get-string)
  (require 'org-habit)
  (require 'org-inlinetask)
  (setq org-habit-graph-column 80)
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
     (sql . t)
     ;; (mysql . t)
     (sqlite . t)
     (clojure . t)
     (java . t)
     (csharp . t)
     ;; (restclient . t)
     ))
  :config
  (org-link-set-parameters
   "attach"
   :follow (lambda (filename)
             (thread-last filename
                          (concat mymy-organization-system-directory-attachments)
                          find-file))
   :help-echo "Filename in the `mymy-organization-system-directory-attachments' directory"
   :face '(:foreground "DarkSeaGreen" :underline t))

  ;; For org roam
  (require 'org-protocol)

  (defun mymy-org-show-headline ()
    "Show like org-cycle does"
    (progn (org-show-entry)
           (org-show-children)))
  (defun mymy-refile-to-done ()
    (interactive)
    (my/refile (concat org-roam-directory "2021-12-05-08-48-44-done.org") "Done"))

  (advice-add 'org-clock-get-clocked-time :around (lambda (f) (if (org-clocking-p) (funcall f) 0)))

  ;; (add-hook 'org-capture-mode-hook #'(lambda () (make-frame) (delete-window)))

  (defvar mymy-org-auto-fill-excluded-elements '(latex-environment latex-fragment link)
    "Elements that shouldn't break line.")


  (el-patch-defun org-auto-fill-function ()
    "Auto-fill function."
    ;; Check if auto-filling is meaningful.
    (let ((fc (current-fill-column)))
      (when (and fc (> (current-column) fc))
        (let* ((fill-prefix (org-adaptive-fill-function))
               ;; Enforce empty fill prefix, if required.  Otherwise, it
               ;; will be computed again.
               (adaptive-fill-mode (not (equal fill-prefix ""))))
          ;; Could use wrap but I'm to lazy for that
          (when (el-patch-remove fill-prefix)
            ;; https://stackoverflow.com/questions/26849364/paragraph-filling-for-org-mode-inside-latex-environment
            (el-patch-add (and (not (memq (org-element-type (org-element-context)) mymy-org-auto-fill-excluded-elements))
                               fill-prefix))
            (do-auto-fill))))))

  ;; FINALLLYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY
  (defun mymy-org-fill-exclude-filling-p ()
    (memq (org-element-type (org-element-context)) mymy-org-auto-fill-excluded-elements))

  (defun mymy-org-fill-hook ()
    (add-to-list 'fill-nobreak-predicate #'mymy-org-fill-exclude-filling-p))

  (add-hook 'org-mode-hook #'mymy-org-fill-hook)

  ;; For latex-math-mode
  ;; (use-package auctex :no-require :config (require 'latex))

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
  (setq org-archive-location "%s_archive::* Archive")
;;; Testing
  (require 'org-depend)
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)  ; turn off logging
      ;; Prefer next because that means that I'm working on the task
      (org-todo (if (= n-not-done 0) "DONE" "NEXT"))))

  (add-hook 'org-after-todo-statistics-hook #'org-summary-todo)
  ;; End of Testings

  ;; To be used outside of emacs
  (defun mymy-org-clock-toggle ()
    (interactive)
    (if (org-clocking-p)
        (org-clock-out)
      (org-clock-in-last)))
  ;; Prefix C-c c
  (general-define-key "C-c c c" 'mymy-org-clock-toggle)

  (defun +org-toggle-inline-image-at-point ()
    "Toggle inline image at point."
    (interactive)
    (if-let* ((bounds (and (not org-inline-image-overlays)
                           (org-in-regexp org-link-any-re nil t)))
              (beg (car bounds))
              (end (cdr bounds)))
        (org-display-inline-images nil nil beg end)
      (org-toggle-inline-images)))

  (defun org-dblock-write:tagcount (params)
    (let ((total))
;;; Get the tags defined in #+TAGS:
      (--> org-current-tag-alist
           ;; Do some preparations in the tag alist
           (--remove (keywordp (car it)) it)
           (-map #'car it)
           ;; Start counting
           (--map (org-count-subentries nil nil it nil (plist-get params :level)) it)
           (--map `(,(car it) ,(1+ (cadr it))) it)
           ;; Keep a record of the total
           (prog1 it
             (setq total (->> it
                              (-map #'cadr)
                              (apply #'+))))
           ;; Do some preparations before
           (--sort (> (cadr it) (cadr other)) it)
           ;; Prepare string representation
           (--map (format "|%s|%s|%.2f|\n"
                          (car it)
                          (cadr it)
                          (* 100 (/ (float (cadr it)) total)))
                  it)
           (push "|-|\n" it)
           (push "|Tags|Count|Percentage|\n" it)
           (push "|-|\n" it)
           (append it (list "|-|\n" (format "|Total|%s|\n" total)
                            "|-|"))
           ;; Insert
           (-map #'insert it)))
    (org-table-align))

  (defun org-dblock-write:block-update-time (params)
    (let ((fmt (or (plist-get params :format) "%d. %m. %Y")))
      (insert "Last block update at: "
              (format-time-string fmt))))

  (defun mymy-org-link-activate-link-alt (start end _path bracketp)
    "Make the link [[PATH][DESC]] show like [DESC]"
    (when bracketp
      (let ((visible-start (or (match-beginning 3) (match-beginning 2)))
            (visible-end (or (match-end 3) (match-end 2))))
        (remove-text-properties start (1+ start) '(invisible nil))
        (remove-text-properties (1- end) end '(invisible nil)))))

  (defun mymy-org-back-to-heading (arg)
    (interactive "P")
    (if (equal arg '(4))
        (outline-up-heading 1)
      (org-back-to-heading)))

  (setq org-latex-pdf-process
        ;; -pdfxe: use xelatex -pdflua: use luatex -bibtex use bibtex when needed
        ;; -xelatex use xelatex for processing files to pdf and turn dvi/ps modes off
        ;; -f: Force -pdf output pdf -bibtex
        ;; (list "latexmk -bibtex -f -pdf %f")
        (list "latexmk -f -pdf -shell-escape -%latex -interaction=nonstopmode -output-directory=%o %f")
        ;; (list "latexmk -f -pdf -shell-escape -xelatex -interaction=nonstopmode -output-directory=%o %f")
        )
  ;; Syntax hightlighting on source code blocs
  ;; https://emacs.stackexchange.com/questions/20839/exporting-code-blocks-to-pdf-via-latex/20841#20841
  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("" "minted"))

  ;; ▶, ▼, ↴, ⬎, ⤷, and ⋱
  (setq org-ellipsis "▶")
  (setq org-log-done t)
  ;; (org-hide-emphasis-markers t)
  (setq org-catch-invisible-edits 'error)
  (setq org-special-ctrl-a/e t)
  (setq org-habit-show-all-today t)
  (setq org-return-follows-link nil)
  ;; In collapsed view, hide empty lines between subtrees
  (setq org-cycle-separator-lines 0)
  ;; Theres seems to be a bug where i can't set new emphasis keywords
  ;; So the only way to set one is overwriting one (org-emphasis-alist
  ;; (btw, i can just modify org-font-lock-extra-keywords but i will not
  ;; get (org-hide-emphasis-markers t) with my current knowledge
  (setq org-emphasis-alist
        '(("*" (bold :foreground "magenta"))
          ("/" italic)
          ("_" underline)
          ("=" org-verbatim org-code)
          ("~" org-code verbatim)
          ("+" (:strike-through t))))
;;; Had this because I was really sensitive to info overload
  ;; (setq org-startup-folded t)
  (setq org-startup-folded 'content)
  (setq org-todo-keyword-faces
        '(("NEXT" . (:foreground "blue" :weight bold))
          ("TODO" . (:foreground "#F09432" :weight bold))
          ("CANCELLED" . (:foreground "red" :weight bold))
          ("SOMEDAY" . (:foreground "#F09432" :weight italics))
          ;; PROJect, as in something without a clear goal
          ;; ("PROJ" . (:foreground "white" :weight bold))
          ))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(s)" "CANCELLED(c)" "SOMEDAY(o)")
          ;; (type "PROJ(p)")
          ))

  (setq org-default-notes-file (f-join mymy-organization-system-directory "agenda.org"))
  (setq org-enforce-todo-dependencies t)
  (setq org-format-latex-options '(plist-put org-format-latex-options :scale 2.0 :background auto :foreground "white"))
  (setq org-highlight-latex-and-related '(latex script entities))
  (setq org-image-actual-width nil)
  (setq org-log-into-drawer t)
  :hook
  (org-mode . org-superstar-mode)
  (org-mode . prettify-symbols-mode)
  ;; (org-mode . org-indent-mode)
  (org-mode . flyspell-mode)
  ;; Use big lines for better compatibility with other viewers
  ;; (org-mode . auto-fill-mode)
  (org-mode . org-super-agenda-mode)
  (kill-emacs . (lambda () (org-clock-out nil t)))
  (org-mode . (lambda () (setq-local tab-width 2
                                     indent-tabs-mode nil
                                     python-shell-interpreter "python3"))))

(use-package org-super-agenda
  :after org
  :ensure t
  :bind (:map org-super-agenda-header-map
              ("n" . org-agenda-next-line)
              ("u" . org-agenda-previous-line)
              :map org-agenda-mode-map
              ("n" . org-agenda-next-line)
              ("u" . org-agenda-previous-line))
  :init
  (setq mymy-org-agenda-tags-width 0)
  (setq org-agenda-custom-commands
        '(("n" "Agenda and all TODOs"
           ((agenda #1="")
            ;; (agenda "" ((org-agenda-overriding-header (mymy-get-count-of-tags))
            ;;             ;; No time grid
            ;;             (org-agenda-time-grid nil)
            ;;             ;; Delete the date
            ;;             (org-agenda-format-date (lambda (x) (ignore x) ""))
            ;;             ))
            (alltodo #1#)))
          ("o" "Agenda for today" ;; agenda ""
           ((todo "TODO"
                  ((org-agenda-overriding-header "")
                   (org-super-agenda-groups
                    '((:name "Homework"
                             :and (:todo ("TODO" "NEXT") :tag "school"))
                      (:discard (:anything t))))))
            (agenda "" ((org-agenda-span 'day)
                        (org-agenda-overriding-header "Today's agenda")))
            (todo "NEXT"
                  ((org-agenda-overriding-header "")
                   (org-super-agenda-groups
                    '((:name "Floating tasks"
                             :and (:todo "NEXT" :scheduled nil))
                      (:discard (:anything t)))))))
           ((org-agenda-compact-blocks nil)))
          ("c" "Inbox/Capture"
           ((todo "" ((org-agenda-files (list (concat mymy-org-roam-dir "inbox/capture.org")))
                      (org-super-agenda-groups
                       '((:name "Up to review"
                                :todo "TODO")
                         (:name "Up to processing"
                                :todo "NEXT")
                         (:name "Waiting for new notes"
                                :todo "STRAY")
                         (:name "To archive"
                                :todo "DONE")))))))))

  ;; Previously called org-agenda-ndays
  ;; (setq org-agenda-span 1)
  ;; (setq org-agenda-span 'week)
  (setq org-agenda-span 8)
  (setq org-agenda-start-on-weekday nil)
  ;; (setq org-agenda-start-day "1d")
  ;; Start two days in the past
  ;; (setq org-agenda-start-day "-2d")
  ;; Start in the present
  (setq org-agenda-start-day "1d")
  (setq org-super-agenda-groups
        '((:name "At Phone"
                 :and (:todo "TODO" :tag "@phone"))
          (:name "Homework"
                 :and (:todo ("TODO" "NEXT") :tag "school"))
          (:name "Pinned to do now"
                 :and (:todo "NEXT" :scheduled t))
          (:name "Schedule of the day"
                 :and (:todo "TODO" :scheduled t))
          ;; :auto-planning
          (:name "Done" :todo "DONE")
          ;; (:discard (:todo "DONE"))
          (:name "Stuck" :anything)
          ;; (:discard (:anything t))
          ))
  :config
  (defun mymy-get-count-of-tags ()
    "Return a string with the counting of tags in the buffer"
    (save-window-excursion
      (org-id-goto "Project_stack")
      (let ((count 0))
        (--reduce (concat acc (if (< count mymy-org-agenda-tags-width)
                                  (progn (setq count (1+ count))
                                         " ")
                                (progn (setq count 0)
                                       "\n"))
                          it )
                  ;; Get a list of the local buffer
                  (--> org-current-tag-alist
                       (-map #'car it)
                       (-remove #'keywordp it)
                       (--map (org-count-subentries nil it nil 1) it)
                       (--remove (< (cadr it) 0) it)
                       (--map (format "%s" it) it)
                       (--map (--> it
                                   (s-chop-prefix "(" it)
                                   (s-chop-suffix ")" it))
                              it))))))
  )

(use-package org-superstar
  :after org
  :ensure t
  :config
  (setq org-superstar-cycle-headline-bullets t)
  (setq org-hide-leading-stars t)
  (setq org-superstar-todo-bullet-alist '(("TODO" . ?☐)
                                          ("NEXT" . ?☐)
                                          ("DONE" . ?☑)))
  (setq org-superstar-special-todo-items t)
  (setq org-superstar-prettify-item-bullets nil)
  (setq org-superstar-headline-bullets-list '(?▹ ?⭆ ?○ ?✸ ?✿ ?✥ ?❂ ?❄)))

(use-package org-pomodoro
  :ensure t
  :demand t
  ;; Works using org mode headings
  :config
  (general-define-key
   :states '(normal motion)
   :keymaps 'override
   :prefix "SPC"
   "m" 'org-pomodoro
   )
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

;; * Text mode utils

;; Put a more comfortable fill-column
(setq-default fill-column 75)

;; Run olivetti, adaptative-wrap and visual-line mode to
(add-hook 'text-mode #'visual-line-mode)

(use-package adaptive-wrap
  :ensure t
  :hook (text-mode . adaptive-wrap-prefix-mode)
  :init
  (setq adaptive-wrap-extra-indent 0)
  )

(use-package olivetti
  :ensure t
  :hook ((olivetti-mode . mymy-configure-olivetti)
         (text-mode . olivetti-mode))
  :init
  (setq fringes-outside-margins t)
  (custom-set-faces
   '(olivetti-fringe ((t (:foreground "#353535" :background "#353535")))))
  (gsetq olivetti-style 'fancy)
  (defun mymy-configure-olivetti ()
    (interactive)
    (setq olivetti-body-width (+ 4 fill-column))))


;; * Flycheck

(use-package flycheck
  :ensure t
  :config
  (gsetq flycheck-indication-mode 'right-fringe)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-u" . flycheck-previous-error)))

;;* Denote

(use-package denote
  :ensure t
  :hook (;; Note: Only works on files that have an identifier on their
         ;; filename
         ;; (find-file . denote-link-buttonize-buffer)
         (dired-mode . denote-dired-mode)
         )
  :init
  ;; Keep it in parallel to main system of orgnization
  (setq denote-directory (concat dropbox-dir "notes/"))
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-project-search-path
                 denote-directory)
    )
  ;; Really easy to upgrade to Markdown and then org mode
  (setq denote-file-type 'text)
  (setq denote-known-keywords
        ;; Description in [[denote:20231229T234559][Index of notes tags]]
        (list "meta" "math" "structure" "software"))

  (defun mymy-denote-find-file ()
    "Call find file on the denote directory"
    (interactive)
    (let ((default-directory denote-directory))
      (call-interactively 'find-file)))

  (defun mymy-denote-link ()
    "Invert behavior of denote-link on link reference"
    (interactive)
    (let ((current-prefix-arg (not current-prefix-arg)))
      (call-interactively #'denote-link)))

  :config

  (general-define-key
   :keymap 'text-mode-map
   "TAB" 'indent-according-to-mode
   )

  (defhydra hydra-denote (:hint nil :exit t)
    "
^Notes^              ^Navigation^         ^Rename^
_n_: Create note     _f_: Find file       _r_: Rename file
_i_: Insert link     _g_: Goto directory  _R_: Rename file using front matter
_o_: Open or create  _w_: Grep

_._: Exit
"
    ("n" denote)
    ;; Call with C-u to insert without description
    ("i" mymy-denote-link)
    ("e" ignore)
    ("o" denote-open-or-create)

    ("f" mymy-denote-find-file)
    ("g" (find-file denote-directory))
    ("p" ignore)
    ("w" (consult-ripgrep denote-directory))

    ("r" denote-rename-file)
    ("R" denote-rename-file-using-front-matter)

    ("." nil :color blue))

  (general-define-key "C-c m" 'hydra-denote/body)
  :hook (after-init . denote-fontify-links-mode)
  )

;; * Todo

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode)
  (keymap-set hl-todo-mode-map "C-c o p" #'hl-todo-previous)
  (keymap-set hl-todo-mode-map "C-c o n" #'hl-todo-next)
  (keymap-set hl-todo-mode-map "C-c o i" #'hl-todo-insert))

(use-package consult-todo
  :ensure (:host github :type git :repo "liuyinz/consult-todo")
  :config
  (keymap-set hl-todo-mode-map "C-c o o" #'consult-todo)
  )

(use-package magit-todos
  :ensure t
  :after magit
  :config (magit-todos-mode 1))

;; * Csharp

(use-package sharper
  :ensure t
  :init
  (general-define-key "C-c b" #'sharper-main-transient)

  (comment
   (defvar mymy-sharper--new-list "dotnet new list"
     "Get a list of all of the avaliable templates.")

   (defconst mymy-sharper--command-get-all-templates "dotnet new list --columns-all | awk -F'  +' 'NR > 4 {printf \"{\\\"TemplateName\\\":\\\"%s\\\",\\\"ShortName\\\":\\\"%s\\\",\\\"Language\\\":\\\"%s\\\",\\\"Type\\\":\\\"%s\\\",\\\"Author\\\":\\\"%s\\\",\\\"Tags\\\":\\\"%s %s %s\\\"}\\n\", $1, $2, $3, $4, $5, $6, $7, $8}' | sed -z '$ s/\\n$//' | tr '\\n' ',' | awk '{print \"[\" $0 \"]\"}'"
     "Shell command that returns the avalibles templates")

   (defun mymy-sharper--new-get-all-options-as-json ()
     (butlast (json-parse-string (shell-command-to-string mymy-sharper--command-get-all-templates)
                                 :object-type 'plist
                                 :array-type 'list)))

   (defun mymy-sharper--new-get-header-names (json)
     (-map (lambda (it) (s-join " " (s-split-words (substring (symbol-name it) 1))))
           (-filter #'keywordp (aref json 0))))

   (defun mymy-sharper--new-get-short-names (json)
     (-map (lambda (it) (plist-get it :ShortName)) json))

   (transient-define-argument mymy-sharper--option-template-short-name ()
     :description "<template-short-name>"
     :class 'transient-option
     :shortarg "T"
     :argument "<template-short-name>="
     :reader (lambda (_prompt _initial-input _history)
               (completing-read
                "Template short name: "
                (->> (mymy-sharper--new-get-all-options-as-json)
                     mymy-sharper--new-get-short-names
                     (-map (lambda (it) (s-split "," it)))
                     -flatten))))

   (comment
    (->> mymy-temp-json-of-templates
         mymy-sharper--new-get-header-names
         (-map (lambda (it) `(,it . 50)))))

   (defconst mymy-sharper--new-template "dotnet new --project %t %o"
     "Common prefix for template commands")

   (defun mymy-sharper--new (&optional transient-params)
     (interactive
      (list (transient-args 'sharper-transient-publish)))
     (transient-set)))

  (defcustom mymy-sharper-framework-lists
    '("net7.0" "net8.0" "net6.0")
    "Target frameworks for dotnet. The framework on the left is used as the default framework")

  (defmacro mymy-shaper--new-transient-prefix (short-name long-name description &rest options)
    (let ((sharper-alias-name (intern (concat "mymy-sharper-new-" short-name)))
          (sharper-transient-prefix-name (intern (concat "mymy-sharper-transient-new-" short-name)))
          )
      `(progn
         (defalias ',sharper-alias-name
           (mymy-shaper-new-- ,short-name ',sharper-transient-prefix-name))

         (transient-define-prefix ,sharper-transient-prefix-name ()
           ,description
           :value `("--use-program-main" ,(concat "--framework=" (car mymy-sharper-framework-lists)))
           ["Options"
            ,@(plist-get options :options)
            ]
           ,@(plist-get options :rest)
           ["Actions"
            ("n" ,(concat "New " long-name) ,sharper-alias-name)
            ("q" "Quit" transient-quit-all)
            ])))
    )

  ;; TODO: Make this work like it should work
;;; For some reason this allows the user to select something even if the
;;; predicate sharper--filename-proj-p returns nil
  ;; (expand-file-name
  ;;  (read-file-name
  ;;   "Select project: "
  ;;   nil
  ;;   nil
  ;;   #'sharper--filename-proj-p
  ;;   nil
  ;;   (lambda (filename)
  ;;     (or (file-directory-p filename)
  ;;         (sharper--filename-proj-p filename)
  ;;         ))
  ;;   )
  ;;  )

  (transient-define-infix mymy-sharper--option-framework ()
    :description "Target framework"
    :class 'transient-option
    :shortarg "-f"
    :argument "--framework="
    :choices mymy-sharper-framework-lists)

  (transient-define-infix mymy-sharper--option-output ()
    :description "Output directory"
    :class 'transient-option
    :shortarg "-o"
    :argument "--output="
    :prompt "Output directory: "
    :always-read t
    :reader (lambda (prompt _initial-input _history)
              (expand-file-name
               (read-file-name
                prompt nil default-directory nil nil #'file-directory-p))))

  (transient-define-infix mymy-sharper--option-csproj ()
    :description ".csproj to use for context"
    :class 'transient-option
    :shortarg "-p"
    :argument "--project="
    :prompt "Output directory: "
    :reader (lambda (_prompt _initial-input _history)
              (sharper--read--project)))

  (transient-define-infix mymy-sharper--option-project-name ()
    :description "Project name"
    :class 'transient-option
    :shortarg "-n"
    :argument "--name="
    :prompt "Project name: "
    :reader (lambda (prompt _ _)
              (read-from-minibuffer
               prompt nil mymy-sharper--project-name-map)))

  (transient-define-infix mymy-sharper--option-force-project-creation ()
    :description "Force project generation"
    :class 'transient-option
    :shortarg "-F"
    :argument "--force")

  (defconst mymy-sharper--project-name-map
    (copy-keymap minibuffer-local-map)
    "Mode map for read-from-minibuffer of Project name")
  (keymap-set mymy-sharper--project-name-map "SPC" 'ignore)
  (keymap-set mymy-sharper--project-name-map "-" 'ignore)

  (defun mymy-sharper-new--create-command (template &optional transient-params)
    (format-spec
     "dotnet new %t %o"
     (list (cons ?t template)
           (cons ?o (s-join " " (-map
                                 (lambda (s)
                                   (string-replace "=" " " s))
                                 transient-params))))))

  (defun mymy-shaper-new-- (template-shortname transient-prefix)
    (lambda (&optional transient-params)
      (interactive
       (list (transient-args transient-prefix)))
      (let ((command (mymy-sharper-new--create-command template-shortname transient-params)))
        (sharper--log-command "New" command)
        (compile command))))

  (defalias 'mymy-sharper-new-console (mymy-shaper-new-- "console" 'mymy-sharper-transient-new-console))

  (transient-define-prefix mymy-sharper-transient-new-console ()
    "Dotnet Console project"
    :value `("--use-program-main" ,(concat "--framework=" (car mymy-sharper-framework-lists)))
    ["Options"
     (mymy-sharper--option-csproj)
     (mymy-sharper--option-project-name)
     (mymy-sharper--option-output)
     (mymy-sharper--option-force-project-creation)
     ]
    ["Template options"
     ("-m" "Use Program class with Main method" "--use-program-main")
     (mymy-sharper--option-framework)]
    ["Actions"
     ("n" "New console" mymy-sharper-new-console)
     ("q" "Quit" transient-quit-all)
     ])

  (defalias 'mymy-sharper-new-web-api (mymy-shaper-new-- "webapi" 'mymy-sharper-transient-new-web-api))

  (transient-define-prefix mymy-sharper-transient-new-web-api ()
    "Dotnet WebApi project"
    :value `("--use-program-main" ,(concat "--framework=" (car mymy-sharper-framework-lists))
             "--use-controllers")
    :incompatible '(("--use-minimal-apis" "--use-controllers"))
    ["Options"
     (mymy-sharper--option-csproj)
     (mymy-sharper--option-project-name)
     (mymy-sharper--option-output)
     (mymy-sharper--option-force-project-creation)
     ]
    ["Template options"
     ("-m" "Use Program class with Main method" "--use-program-main")
     ("-c" "Use controllers" "--use-controllers")
     ("-i" "Use minimal apis" "--use-minimal-apis")
     ("-O" "No OpenAPI suppor (Swagger)" "--no-openapi")
     ("-au" "The type of authentication to use" "--auth="
      :choices ("None"
                "IndividualB2C"
                "SingleOrg"
                "Windows")
      )
     (mymy-sharper--option-framework)
     ]
    ["Actions"
     ("n" "New web API" mymy-sharper-new-web-api)
     ("q" "Quit" transient-quit-all)
     ])

  (defalias 'mymy-sharper-new-nunit (mymy-shaper-new-- "nunit" 'mymy-sharper-transient-new-nunit))

  (transient-define-prefix mymy-sharper-transient-new-nunit ()
    "Dotnet Nunit project"
    :value `(,(concat "--framework=" (car mymy-sharper-framework-lists)))
    ["Options"
     (mymy-sharper--option-csproj)
     (mymy-sharper--option-project-name)
     (mymy-sharper--option-output)
     (mymy-sharper--option-force-project-creation)
     ]
    ["Template options"
     (mymy-sharper--option-framework)
     ]
    ["Actions"
     ("n" "New NUnit 3 Test Project" mymy-sharper-new-nunit)
     ("q" "Quit" transient-quit-all)
     ])

  (defalias 'mymy-sharper-new-editorconfig
    (mymy-shaper-new-- "editorconfig" 'mymy-sharper-transient-new-editorconfig))

  (transient-define-prefix mymy-sharper-transient-new-editorconfig ()
    "Dotnet .editorconfig item"
    :value `(,(concat "--framework=" (car mymy-sharper-framework-lists)))
    ["Options"
     (mymy-sharper--option-csproj)
     (mymy-sharper--option-project-name)
     (mymy-sharper--option-output)
     (mymy-sharper--option-force-project-creation)
     ]
    ["Template options"
     ("-e" "Create empty instead of .NET defaults" "--empty")
     ]
    ["Actions"
     ("n" "New editorconfig" mymy-sharper-new-editorconfig)
     ("q" "Quit" transient-quit-all)
     ])

  (defalias 'mymy-sharper-new-gitignore
    (mymy-shaper-new-- "gitignore" 'mymy-sharper-transient-new-gitignore))

  (transient-define-prefix mymy-sharper-transient-new-gitignore ()
    "Dotnet .gitignore item"
    :value `(,(concat "--framework=" (car mymy-sharper-framework-lists)))
    ["Options"
     (mymy-sharper--option-csproj)
     (mymy-sharper--option-project-name)
     (mymy-sharper--option-output)
     (mymy-sharper--option-force-project-creation)
     ]
    ["Actions"
     ("n" "New .gitignore" mymy-sharper-new-gitignore)
     ("q" "Quit" transient-quit-all)
     ])

  (mymy-shaper--new-transient-prefix
   "classlib"
   "Class library"
   "Dotnet Class library project"
   :options
   ((mymy-sharper--option-csproj)
    (mymy-sharper--option-project-name)
    (mymy-sharper--option-output)
    (mymy-sharper--option-force-project-creation))
   )

  (mymy-shaper--new-transient-prefix
   "sln"
   "Solution file"
   "Solution item"
   :options
   ((mymy-sharper--option-csproj)
    (mymy-sharper--option-project-name)
    (mymy-sharper--option-output)
    (mymy-sharper--option-force-project-creation))
   )

  (mymy-shaper--new-transient-prefix
   "xunit"
   "xUnit Test Project"
   "xUnit Test Project"
   :options
   ((mymy-sharper--option-csproj)
    (mymy-sharper--option-project-name)
    (mymy-sharper--option-output)
    (mymy-sharper--option-force-project-creation))
   )

  (mymy-shaper--new-transient-prefix
   "mvc" ()
   "ASP.NET Core Web App (MVC)"
   "ASP.NET Core Web App Project"
   :options
   ((mymy-sharper--option-csproj)
    (mymy-sharper--option-project-name)
    (mymy-sharper--option-output)
    (mymy-sharper--option-force-project-creation))
   :rest
   (["Template options"
     ("-m" "Use Program class with Main method" "--use-program-main")
     ("-au" "The type of authentication to use" "--auth="
      :choices ("None"
                "IndividualB2C"
                "SingleOrg"
                "Windows")
      )
     (mymy-sharper--option-framework)
     ])
   )

  (mymy-shaper--new-transient-prefix
   "webapp"
   "ASP.NET Core Web App (Razor Pages)"
   "ASP.NET Core Web App Project"
   :options
   ((mymy-sharper--option-csproj)
    (mymy-sharper--option-project-name)
    (mymy-sharper--option-output)
    (mymy-sharper--option-force-project-creation))
   :rest
   (["Template options"
     ("-m" "Use Program class with Main method" "--use-program-main")
     ("-au" "The type of authentication to use" "--auth="
      :choices ("None"
                "IndividualB2C"
                "SingleOrg"
                "Windows")
      )
     (mymy-sharper--option-framework)
     ])
   )


  (transient-define-prefix mymy-sharper-transient-new ()
    "Dotnet templates"
    ["Projects templates"
     ("Co" "Console" mymy-sharper-transient-new-console)
     ("Wa" "ASP.NET Core Web API" mymy-sharper-transient-new-web-api)
     ("Wp" "ASP.NET Core Web App (MVC)" mymy-sharper-transient-new-mvc)
     ("Wr" "ASP.NET Core Web App (Razor Pages)" mymy-sharper-transient-new-webapp)
     ("Tn" "NUnit 3 Test Project" mymy-sharper-transient-new-nunit)
     ("Tx" "xUnit Test Project" mymy-sharper-transient-new-xunit)
     ("Cl" "Classlib Project" mymy-sharper-transient-new-classlib)
     ]
    ["Item templates"
     ("g" "dotnet gitignore file" mymy-sharper-transient-new-gitignore)
     ("e" "EditorConfig file" mymy-sharper-transient-new-editorconfig)
     ("s" "Solution file" mymy-sharper-transient-new-sln)
     ])

  :config
  (transient-insert-suffix
    #'sharper-main-transient "c"
    '("w" "new" mymy-sharper-transient-new)))

(use-package csproj-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.csproj\\'" . csproj-mode))
  (add-hook 'csproj-mode-hook #'aggressive-indent-mode)
  )

(use-package csharp-ts-mode
  :no-require t
  :ensure nil
  :config
  (defun mymy-csharp-mode-hook ()
    (setq-local flycheck-navigation-minimum-level 'error)
    )
  (add-hook 'csharp-ts-mode-hook #'mymy-csharp-mode-hook)
  )

;; * Git gutter
(use-package diff-hl
  :ensure t
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [224] nil nil '(center repeated))

  (setq vc-git-diff-switches '("--histogram"))

  (global-diff-hl-mode)

  (add-hook 'diff-hl-mode-hook #'diff-hl-flydiff-mode)

  ;; Customizable fringe bitmap?
  ;; https://github.com/dgutov/diff-hl/issues/116

  (comment
   The package also contains auxiliary modes:

   `diff-hl-dired-mode' provides similar functionality in Dired.

   `diff-hl-margin-mode' changes the highlighting function to use the
   margin instead of the fringe.

   `diff-hl-amend-mode' sets the reference revision to the one before
   recent one. Also, you could use diff-hl-set-reference-rev to set it to
   any revision, see its docstring for details.

   `diff-hl-flydiff-mode' implements highlighting changes on the fly.

   `diff-hl-show-hunk-mouse-mode' makes fringe and margin react to mouse
   clicks to show the corresponding hunk. That's the alternative to using
   diff-hl-show-hunk and friends.
   )
  )

;; * Ripgrep
(use-package ag
  :ensure t)


;; * Multiple cursors

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))


;; * CSV mode

(use-package csv-mode
  :ensure t
  :hook (csv-mode . csv-align-mode)
  )


;; * Common lisp

(use-package sly
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl --dynamic-space-size 1024")
  (setq sly-lisp-implementations
        '((sbcl ("sbcl" "--dynamic-space-size" "1024")))))