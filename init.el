;; -*- lexical-binding: t; outline-regexp: ";; \\*+"; -*-

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

(defvar elpaca-installer-version 0.8)
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
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
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

(when (eq system-type 'windows-nt)
  ;; Windows has limit of like 512 handles
  (setq elpaca-queue-limit 10)
  )

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(defun mymy-is-android ()
  (equal system-configuration
         "aarch64-unknown-linux-android"))
(setq mymy-is-android (mymy-is-android))
(setq mymy-is-not-android (not (mymy-is-android)))

(use-package use-package
  :config
  ;; Copy of consult--outline-candidates
  (defun mymy-consult-use-package--candidates ()
    "Return alist of use-package declarations and positions."
    (consult--forbid-minibuffer)

    (let ((buffer (current-buffer))
          (line (line-number-at-pos (point-min) consult-line-numbers-widen))
          candidates)
      (save-excursion
        (goto-char (point-min))
        (while (save-excursion
                 (re-search-forward "^\s*(use-package" nil t))
          (cl-incf line (consult--count-lines (match-beginning 0)))
          (push (consult--location-candidate
                 (consult--buffer-substring (pos-bol) (pos-eol) 'fontify)
                 (cons buffer (point)) (1- line) (1- line))
                candidates)
          (goto-char (1+ (pos-eol)))))
      (unless candidates
        (user-error "No use package declarations"))
      (nreverse candidates)))

  (defun mymy-consult-use-package ()
    "Jump to a use-package declaration location."
    (interactive)
    (let ((candidates
           (consult--slow-operation
               "Collecting use-package declarations..."
             (mymy-consult-use-package--candidates))))
      (consult--read
       candidates
       :prompt "Go to package: "
       :annotate (consult--line-prefix)
       :category 'consult-location
       :sort nil
       :require-match t
       :lookup #'consult--line-match
       :history '(:input consult--line-history)
       :add-history (thing-at-point 'symbol)
       :state (consult--location-state candidates))))

  (define-key
   emacs-lisp-mode-map
   (kbd "C-c h u")
   #'mymy-consult-use-package)
  )

(require 'cl-lib)

;; Make use package verbose on debug-init
(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

(use-package display-line-numbers
  :config
  (defun mymy-display-line-numbers-configure ()
    (setq-local display-line-numbers 'relative)
    ;; Do not show the current line when relative
    (setq display-line-numbers-current-absolute nil)
    )
  :hook
  (prog-mode . mymy-display-line-numbers-configure)
  (text-mode . mymy-display-line-numbers-configure)
  )

(when mymy-is-android
  (load "mwheel")
  (mwheel-install)
  (mouse-wheel-mode 1)
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-wheel-scroll-amount
        '(1 ((shift) . 1)
            ((control) . 5)))

  (setq mouse-wheel-tilt-scroll t)
  ;; (global-set-key [C-right] [mouse-7])
  ;; (global-set-key [C-left] [mouse-6])

  (setq mouse-wheel-flip-direction 'wheel-right)
  ;; (with-eval-after-load 'org
  ;;   (add-hook 'org-agenda-mode-hook
  ;;             #'visual-line-mode)
  ;;   )
  (with-eval-after-load 'general
    (general-define-key
     "M-h" (lambda ()
             (interactive)
             (evil-scroll-column-left 10))
     "M-l" (lambda ()
             (interactive)
             (evil-scroll-column-right 10))
     )
    )
  )

(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)

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
  (setq read-process-output-max (* 1024 1024))       ;; 1mb
  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1)) ; This is much easier
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ; than needing to change
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ; this on every OS
  (setq byte-compile-warnings '(not obsolete))           ;; Cl warnings
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t)
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq create-lockfiles nil)
  (setq abbrev-suggest t)
  ;; I finally caught on. This is annoying when it tries to.
  (setq require-final-newline nil)
  (setq mode-require-final-newline nil)
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
  (setq use-dialog-box nil)             ; Text-based options are better
;;; Seems this is not supported anymore, whatever it did
  ;; (setq bidi-display-reordering nil)
  (setq bidi-inhibit-bpa t)
  (setq-default bidi-paragraph-direction 'left-to-right)

                                        ; Why this varible even exists?
  ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2020-09/msg01922.html
  (setq delete-pair-blink-delay 0)
  (setq font-lock-verbose nil)
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
      (setq default-input-method "japanese-mozc")))
  (setq-default tab-always-indent t)
  (setq-default whitespace-line-column 1000)
  (setq-default cursor-type '(bar . 2)) ;; Change cursor to a bar
  (setq-default cursor-in-non-selected-windows nil)
  (setq-default blink-cursor-blinks 0)  ; Never stop to blink
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
  (setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo"))))
  ;; Emacs doing one of its shennanigans again, if you subscribe to renaming
  ;; then this little gremling will do some moving that will trick your
  ;; subscribers into thinking the file move there, when it is just emacs
  ;; trickery
  (setq backup-by-copying t)

  (savehist-mode)

  (setq savehist-additional-variables '(register-alist))
  (setq split-height-threshold 80
        split-width-threshold 160
        split-window-preferred-function 'split-window-sensibly)

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
  ;; (when (timerp undo-auto-current-boundary-timer)
  ;;   (cancel-timer undo-auto-current-boundary-timer))

  ;; Limit of 64mb.
  (setq undo-limit 6710886400)
  ;; Strong limit of 1.5x (96mb)
  (setq undo-strong-limit 100663296)
  ;; Outer limit of 10x (960mb).
  ;; Note that the default is x100), but this seems too high.
  (setq undo-outer-limit 1006632960)

  (global-hl-line-mode)
  ;; * Set the font
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

  ;; * Load path


  (add-to-list 'load-path (concat user-emacs-directory "lib/"))

  ;; * Lib Requeries
  (require 'functions.el)
  (require 'vars.el)
  (require 'spec-keywords)

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
              ;; " "(format-time-string "%H:%M")
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
  (add-to-list 'default-frame-alist
               '(undecorated . t))

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

  (defvar mymy-readonly-directories
    '())

  (with-eval-after-load 'straight
    (add-to-list 'mymy-readonly-directories (straight--dir)))

  (with-eval-after-load 'elpaca
    (add-to-list 'mymy-readonly-directories elpaca-directory))

  (defun mymy-hook-to-read-only-in-selected-dirs ()
    "Open files under straight as read-only"
    ;; (when (string-match-p (straight--dir) (buffer-file-name))
    ;;   (read-only-mode 1))
    (when (-first (lambda (d) (string-match-p d (buffer-file-name)))
                  mymy-readonly-directories)
      (read-only-mode 1)))

  (add-hook 'find-file-hook #'mymy-hook-to-read-only-in-selected-dirs)

  (setq dired-vc-rename-file t)
  (setq completion-ignore-case t)
  (global-set-key (kbd "M-g M-o") 'consult-outline)
  (global-set-key (kbd "M-g o") 'consult-outline)
  ;; Add support for ansi color output from compilation output.
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
  ;; Increase the size of the history to make it smarter.
  (setq history-length 1000)
  (with-eval-after-load 'general
    (general-define-key
     :states '(motion normal visual)
     :keymaps 'help-mode-map
     "gd" #'elisp-slime-nav-find-elisp-thing-at-point
     "K" #'elisp-slime-nav-describe-elisp-thing-at-point
     )
    )

  ;; (add-to-list 'display-buffer-alist
  ;;              '("\\*Help"
  ;;                (display-buffer-in-direction)
  ;;                (direction . right)
  ;;                (window-width . 80)))
  (defun mymy-mode-line-word-with-padding (word &optional padding)
    (let* ((half (/ (window-total-width) 2))
           (side-length (- half (/ (length word) 2)))
           (paddings (make-string side-length (or padding ?-))))
      (concat
       paddings
       word
       paddings))
    )

  (add-to-list 'display-buffer-alist '("\\*WoMan *"
                                       (display-buffer-reuse-window display-buffer-in-direction)
                                       (direction . bottom)
                                       (window-height . 0.4)
                                       ))

  (global-set-key (kbd "C-{") #'evil-newline-same-indent)
  (unless window-system
    (when (getenv "DISPLAY")
      (defun xsel-cut-function (text &optional push)
        (with-temp-buffer
          (insert text)
          (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--input" "--clipboard")))
      (defun xsel-paste-function()
        (let ((xsel-output (shell-command-to-string "xsel --output --clipboard")))
          (unless (string= (car kill-ring) xsel-output)
            xsel-output )))
      (setq interprogram-cut-function 'xsel-cut-function)
      (setq interprogram-paste-function 'xsel-paste-function)
      ))
  )

(use-package saveplace
  :init
  (save-place-mode 1)
  :custom
  (save-place-ignore-files-regexp
   "\\(?:COMMIT_EDITMSG\\|hg-editor-[[:alnum:]]+\\.txt\\|elpa\\|svn-commit\\.tmp\\|bzr_log\\.[[:alnum:]]+\\)$")
  (save-place-forget-unreadable-files t))

(use-package lsp-c
  :after (lsp)
  :ensure nil
  :no-require t

  ;; :hook
  ;; (c-mode . lsp)
  ;; (c++-mode . lsp)
  ;; (c++-ts-mode . lsp)
  ;; (c-ts-mode . lsp)
  )

(use-package elisp-slime-nav
  :ensure t
  :init
  ;; Evil collection already defines the bindings
  ;; (general-define-key
  ;;  :states '(normal visual motion)
  ;;  :keymaps 'emacs-lisp-mode-map
  ;;  :prefix "SPC"
  ;;  "gd" #'elisp-slime-nav-find-elisp-thing-at-point
  ;;  )
  (defun mymy-emacs-lisp-hook ()
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
  (emacs-lisp-mode . aggressive-indent-mode)
  (lisp-mode . aggressive-indent-mode))

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

;; Some file utitlities
(use-package f
  :ensure f)

(use-package seq :ensure t)

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
   "C-c C-e" nil)
  (general-define-key
   "C-x C-y" 'pp-macroexpand-last-sexp))

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
        (ol8 '(:height 1.0 :weight bold :slant italic :foreground "#077ffa")))
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
    :group 'basic-faces))
;; Sync shell env variables to emacs env variables
(use-package exec-path-from-shell
  :ensure t
  :demand t
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
  (setq evil-respect-visual-line-mode t)
  :config
  (evil-mode)
  (gsetq evil-undo-system 'undo-tree)
  (gsetq evil-want-fine-undo t)
  (general-def 'normal emacs-lisp-mode-map
    "K" 'elisp-slime-nav-describe-elisp-thing-at-point)

  (general-override-mode 1)
  ;; (define-key evil-emacs-state-map (kbd "S-SPC") 'evil-normal-state)

  ;; It get's kind of annoying, maybe will activate later.
  (gsetq evil-want-empty-ex-last-command nil)

  (defvar mymy-buffer-map
    (-doto (make-sparse-keymap)
      (define-key (kbd "b") #'switch-to-last-buffer)
      (define-key (kbd "d") #'kill-current-buffer)))

  (defvar mymy-find-leader-key "f")

  (defvar mymy-find-map
    (-doto (make-sparse-keymap)
      (define-key (kbd "b") #'consult-buffer)
      ;; (define-key (kbd "f") #'find-file)
      (define-key (kbd ".") #'find-file)
      (define-key (kbd "x") #'reopen-killed-file)
      (define-key (kbd "X") #'reopen-killed-file-fancy)
      (define-key (kbd "l") #'consult-line)
      (define-key (kbd "g") #'mymy-consult-grep-change-depending-on-arg)
      (define-key (kbd "s") #'describe-symbol)))

  (defvar mymy-flycheck-map
    (-doto (make-sparse-keymap)
      (define-key (kbd "c") #'flycheck-buffer)
      (define-key (kbd "e") #'flycheck-explain-error-at-point)
      (define-key (kbd "l") #'flycheck-list-errors)
      (define-key (kbd "x") #'flycheck-disable-checker)
      (define-key (kbd "m") #'flycheck-mode)))

  (defvar mymy-replace-map
    (-doto (make-sparse-keymap)
      (define-key (kbd "s") #'replace-string)
      (define-key (kbd "r") #'replace-regexp)))

  ;; @Evil define key
  (general-define-key
   :states '(normal motion visual)
   :keymaps 'override
   :prefix "SPC"

   "n" #'make-frame-command
   "b" (list mymy-buffer-map :which-key "Buffer")
   mymy-find-leader-key (list mymy-find-map :which-key "Find")
   "!" (list mymy-flycheck-map :which-key "Flycheck")
   "r" (list mymy-replace-map :which-key "Replace")
   "m" #'magit)

  (defun meain/evil-yank-advice (orig-fn beg end &rest args)
    (pulse-momentary-highlight-region beg end)
    (apply orig-fn beg end args))

  (advice-add 'evil-yank :around 'meain/evil-yank-advice)

  ;; Change shape and color of each state
  (setq esvil-insert-state-cursor '(bar "#00FF00")
        evil-visual-state-cursor '(box "#FF00FF")
        evil-normal-state-cursor '(hollow "#E2E8EF")))


;; Extensions with evil and others
(use-package evil-collection
  :after evil
  :ensure t
  ;; :ensure (evil-collection :host github :repo "emacs-evil/evil-collection")
  :config
  ;; (with-eval-after-load 'pdf-tools
  ;;   (evil-collection-init '(pdf))
  ;;   )

  ;; For some reason not being loaded
  (evil-collection-pdf-setup)
  (evil-collection-init '(dired consult corfu
                                elisp-slime-nav elisp-mode
                                debug help
                                magit magit-section magit-repos
                                magit-todos
                                org
                                vertico
                                wgrep wdired
                                flycheck
                                bookmark
                                ;; vterm
                                pdf
                                compile
                                comint
                                sly)))

;; Integration of lispy with evil
(use-package lispyville
  :ensure t
  :after evil lispy
  :init
  (general-add-hook '(emacs-lisp-mode-hook lisp-mode-hook) #'lispyville-mode)
  :config
  (advice-add 'lispyville-yank :around 'meain/evil-yank-advice)
  (lispyville-set-key-theme '(operators c-w additional)))

(use-package evil-surround
  :ensure t
  :demand t
  :config
  (global-evil-surround-mode 1))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (setq evil-org-special-o/O '(table-row)))

(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode 1))

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
        ;; (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        ;; Having problems with identation, see https://github.com/llemaitre19/jtsx/issues/12
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.3" "src")
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
        (nix "https://github.com/nix-community/tree-sitter-nix")
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
        which-key-window-max-width 0.66)
  :config
  (which-key-mode))

;; * Lsp mode
(when mymy-is-not-android
  (use-package lsp-mode
    :ensure t
    :init
    (setq lsp-keymap-prefix "C-c l")
    (setq lsp-disabled-clients
          '(semgrep-ls emmet-ls))
    (setq lsp-auto-execute-action nil)
    (setq lsp-completion-provider :none)
    ;; (setq lsp-signature-function #'lsp-signature-posframe)
    (setq lsp-signature-function #'lsp-lv-message)
    (setq lsp-semantic-tokens-enable nil)
    :config
    (setq lsp-eldoc-enable-hover t)
    (setq lsp-modeline-code-actions-enable nil)
    (setq lsp-modeline-diagnostics-enable nil)
    (setq lsp-modeline-workspace-status-enable nil)
    (setq lsp-signature-doc-lines 1)

    ;; (define-key lsp-mode-map (kbd "M-RET") #'lsp-execute-code-action)
    (define-key lsp-mode-map (kbd "M-?") #'lsp-find-references)
    (define-key lsp-mode-map (kbd "M-/") #'lsp-find-implementation)
    (define-key lsp-mode-map (kbd "M-.") #'lsp-find-definition)
    ;; (define-key lsp-mode-map (kbd "C-;") #'lsp-iedit-highlights)
    (define-key lsp-mode-map (kbd "C-M-;") #'lsp-iedit-highlights)
    ;; (define-key lsp-signature-mode-map (kbd "M-N") #'lsp-signature-next)
    ;; (define-key lsp-signature-mode-map (kbd "M-p") #'lsp-signature-previous)
    (define-key lsp-signature-mode-map (kbd "C-M-n") #'lsp-signature-next)
    (define-key lsp-signature-mode-map (kbd "C-M-p") #'lsp-signature-previous)
    ;; (define-key lsp-signature-mode-map (kbd "M-n") #'lsp-signature-next)
    ;; (define-key lsp-signature-mode-map (kbd "M-p") #'lsp-signature-previous)


    ;; https://www.reddit.com/r/emacs/comments/ql8cyp/corfu_orderless_and_lsp/?rdt=40464
    (defun corfu-lsp-setup ()
      (setq-local completion-category-defaults nil))
    (add-hook 'lsp-mode-hook #'corfu-lsp-setup)



    (when (executable-find "emacs-lsp-booster")
      (defun lsp-booster--advice-json-parse (old-fn &rest args)
        "Try to parse bytecode instead of json."
        (or
         (when (equal (following-char) ?#)
           (let ((bytecode (read (current-buffer))))
             (when (byte-code-function-p bytecode)
               (funcall bytecode))))
         (apply old-fn args)))
      (advice-add (if (progn (require 'json)
                             (fboundp 'json-parse-buffer))
                      'json-parse-buffer
                    'json-read)
                  :around
                  #'lsp-booster--advice-json-parse)

      (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
        "Prepend emacs-lsp-booster command to lsp CMD."
        (let ((orig-result (funcall old-fn cmd test?)))
          (if (and (not test?) ;; for check lsp-server-present?
                   (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
                   lsp-use-plists
                   (not (functionp 'json-rpc-connection)) ;; native json-rpc
                   (executable-find "emacs-lsp-booster"))
              (progn
                (when-let ((command-from-exec-path (executable-find (car orig-result)))) ;; resolve command from exec-path (in case not found in $PATH)
                  (setcar orig-result command-from-exec-path))
                (message "Using emacs-lsp-booster for %s!" orig-result)
                (cons "emacs-lsp-booster" orig-result))
            orig-result)))
      (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
      )

    (require 'lsp-angular)
    (setq lsp-clients-angular-language-server-command
          '("node"
            "/usr/lib/node_modules/@angular/language-server"
            "--ngProbeLocations"
            "/usr/lib/node_modules"
            "--tsProbeLocations"
            "/usr/lib/node_modules"
            "--stdio"))
    :hook (web-mode . lsp)

    )
  )

(when mymy-is-not-android
  (use-package lsp-vue
    :after (lsp-mode)
    :ensure nil
    :no-require t
    :config
    ;; It seems that now disabling talsp-volar-take-over-modeke over is recommended
    (setq lsp-volar-take-over-mode nil)
    ;; (require 'lsp-volar)
    ;; ;; Also takes over in non vue projects, which is kind of annoying.
    ;; (el-patch-defun lsp-volar--activate-p (filename &optional _)
    ;;   "Check if the volar-language-server should be enabled base on FILENAME."
    ;;   (if lsp-volar-take-over-mode
    ;;       (or (or
    ;;            (and (lsp-workspace-root) (lsp-volar--vue-project-p (lsp-workspace-root)))
    ;;            (and (lsp-workspace-root) lsp-volar-activate-file (f-file-p (f-join (lsp-workspace-root) lsp-volar-activate-file))))
    ;;           (or (el-patch-remove
    ;;                 ;; Why would I want a volar server in a non vue project?
    ;;                 (or (string-match-p "\\.mjs\\|\\.[jt]sx?\\'" filename)
    ;;                     (and (derived-mode-p 'js-mode 'typescript-mode 'typescript-ts-mode)
    ;;                          (not (derived-mode-p 'json-mode)))))
    ;;               (string= (file-name-extension filename) "vue")))
    ;;     (string= (file-name-extension filename) "vue")))
    ))

;; (use-package lsp-snippet-tempel
;;   :after tempel
;;   :ensure (lsp-snippet-tempel :type git
;;                               :host github
;;                               :repo "svaante/lsp-snippet")
;;   :config
;;   (when (featurep 'lsp-mode)
;;     ;; Initialize lsp-snippet -> tempel in lsp-mode
;;     (lsp-snippet-tempel-lsp-mode-init))
;;   (when (featurep 'eglot)
;;     ;; Initialize lsp-snippet -> tempel in eglot
;;     (lsp-snippet-tempel-eglot-init)))

(use-package evil-lsp
  :after evil lsp-mode
  :ensure nil
  :no-require t
  :config
  (general-define-key
   :states '(normal motion visual)
   :keymaps 'lsp-mode-map
   :prefix "g"

   "r" 'lsp-find-references
   "d" 'lsp-find-definition
   "D" 'lsp-find-implementation)
  (general-define-key
   :states '(normal motion visual)
   :keymaps 'lsp-mode-map
   :prefix "SPC"

   (concat mymy-find-leader-key "r") 'lsp-find-references
   (concat mymy-find-leader-key "d") 'lsp-find-definition
   (concat mymy-find-leader-key "D") 'lsp-find-implementation

   "l" lsp-command-map)
  (general-define-key
   :states '(normal motion)
   :keymaps 'lsp-mode-map
   ;; "K" 'lsp-describe-thing-at-point
   "K" 'lsp-ui-doc-show))

(when mymy-is-not-android
  (use-package lsp-pyright
    :ensure t
    :init
    (defun mymy-python-lsp-hook ()
      (require 'lsp-pyright)
      (lsp))
    :hook
    (python-mode . mymy-python-lsp-hook)
    (python-ts-mode . mymy-python-lsp-hook)))

(use-package lsp-omnisharp
  :after (lsp-mode)
  :ensure nil
  :no-require t
  :config
  (setq
   lsp-csharp-server-path
   (expand-file-name (concat dropbox-dir "omnisharp/net6.0/OmniSharp"))
   )
  (lsp-register-client
   (make-lsp-client :new-connection
                    (lsp-stdio-connection
                     #'(lambda ()
                         (append
                          (list (lsp-csharp--language-server-path) "-lsp"
                                ;; "-l" "Debug"
                                ;; "--loglevel"
                                ;; "Trace"
                                )
                          ;; (when lsp-razor-rzls-test-dll
                          ;;   (list "--plugin" lsp-razor-rzls-test-dll)
                          ;;   )
                          (when lsp-csharp-solution-file
                            (list "-s" (expand-file-name lsp-csharp-solution-file)))))
                     #'(lambda ()
                         (when-let ((binary (lsp-csharp--language-server-path)))
                           (f-exists? binary))))
                    :activation-fn (lsp-activate-on "csharp" "aspnetcorerazor")
                    :server-id 'omnisharp-razor
                    :priority 0
                    :uri->path-fn #'lsp-csharp--omnisharp-uri->path-fn
                    :action-handlers (ht ("omnisharp/client/findReferences" 'lsp-csharp--action-client-find-references))
                    :notification-handlers (ht ("o#/projectadded" 'ignore)
                                               ("o#/projectchanged" 'ignore)
                                               ("o#/projectremoved" 'ignore)
                                               ("o#/packagerestorestarted" 'ignore)
                                               ("o#/msbuildprojectdiagnostics" 'ignore)
                                               ("o#/packagerestorefinished" 'ignore)
                                               ("o#/unresolveddependencies" 'ignore)
                                               ("o#/error" 'lsp-csharp--handle-os-error)
                                               ("o#/testmessage" 'lsp-csharp--handle-os-testmessage)
                                               ("o#/testcompleted" 'lsp-csharp--handle-os-testcompleted)
                                               ("o#/projectconfiguration" 'ignore)
                                               ("o#/projectdiagnosticstatus" 'ignore)
                                               ("o#/backgrounddiagnosticstatus" 'ignore)
                                               )
                    :download-server-fn #'lsp-csharp--omnisharp-download-server))

  (setenv "DOTNET_RUNTIME_ID" "linux-x64")
  :hook (csharp-ts-mode . lsp))

(use-package lsp-razor
  :disabled
  :after (lsp-mode web-mode)
  :ensure nil
  :load-path "lsp-razor.el"
  :init
  (add-to-list 'lsp-language-id-configuration '(razor-web-mode . "aspnetcorerazor"))

  (defun mymy-lsp-razor-hook ()
    (setq-local comment-start "@*")
    (setq-local comment-end "*@")
    (lsp))

  ;; (add-to-list 'treesit-extra-load-path "~/.cache/tree-sitter/lib/")
  ;; (add-to-list 'treesit-load-name-override-list '(razor "razor"))

  (require 'lsp-razor)

  :hook
  (razor-web-mode . mymy-lsp-razor-hook))

(when mymy-is-not-android
  (use-package lsp-java
    :ensure t
    :config
    (add-hook 'java-mode-hook 'lsp)
    (add-hook 'java-ts-mode-hook 'lsp)

    (defconst mymy-lsp-java-jvm-locations "/usr/lib/jvm"
      "Path to the directory with all java sdk"
      )

    (gsetq
     lsp-java-java-path
     (expand-file-name
      "bin/java"
      (getenv "JAVA_HOME")
      )
     )

    (gsetq
     lsp-java-import-gradle-java-home
     (getenv "JAVA_HOME")
     )

    ;; (gsetq
    ;;  lsp-java-configuration-runtimes
    ;;  []
    ;;  )

    (gsetq
     lsp-java-configuration-runtimes
     [( :name "JavaSE-17"
        :path "/usr/lib/jvm/java-17-openjdk"
        :default t)
      ( :name "JavaSE-11"
        :path "/usr/lib/jvm/java-11-openjdk")
      ( :name "JavaSE-1.8"
        :path "/usr/lib/jvm/java-8-openjdk")
      ;; ( :name "JavaSE-21"
      ;;   :path "/usr/lib/jvm/java-21-openjdk")
      ;; ( :name "JavaSE-22"
      ;;   :path "/usr/lib/jvm/java-22-openjdk")
      ])

    (gsetq
     lsp-java-compile-null-analysis-mode
     "interactive "
     )
    (gsetq lsp-java-imports-gradle-wrapper-checksums
           [( :sha256 "ebb6eaf164c425ffe76f9744a324feb774e750d821ed212d4c41f452adea248e"
              :allowed t)
            ]
           )
    ))

(when mymy-is-not-android
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
    ;; (setq lsp-haskell-server-args `("-d" "-l" ,lsp-haskell-server-log-file))
    (setq lsp-haskell-server-path (expand-file-name "~/.ghcup/bin/haskell-language-server-wrapper"))))


(comment
 (lsp-register-client
  (make-lsp--client
   :new-connection (lsp-stdio-connection (lambda () (expand-file-name "~/.cabal/bin/static-ls")))
   ;; Should run under haskell-mode, haskell-literate-mode and haskell-tng-mode. We need to list haskell-literate-mode even though it's a derived mode of haskell-mode.
   :major-modes '(haskell-mode haskell-literate-mode haskell-tng-mode haskell-cabal-mode)
   ;; This is arbitrary.
   :server-id 'static-ls-haskell
   ;; :synchronize-sections '("haskell")
   ;; This is somewhat irrelevant, but it is listed in lsp-language-id-configuration, so
   ;; we should set something consistent here.
   :language-id "haskell"
   ;; :completion-in-comments? lsp-haskell-completion-in-comments
   ;; :action-filter #'lsp-haskell--action-filter
   :priority 1
   )))

(when mymy-is-not-android
  (add-hook 'html-mode-hook #'lsp)
  (add-hook 'tsx-ts-mode-hook #'lsp)
  (add-hook 'typescript-ts-mode-hook #'lsp)
  (add-hook 'js-mode-hook #'lsp)
  (add-hook 'js-ts-mode-hook #'lsp)
  )

(when mymy-is-not-android
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
     "C-x C-e" #'mymy-dap-eval-dwim)

    ;; (setq dap-internal-terminal #'dap-internal-terminal-vterm)

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
                                              ("COMPLUS_ReadyToRun" . "0"))))

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
                                       :env '(("COMPLUS_ReadyToRun" . "0"))))

    ;; If I run dotnet test with env VSTEST_HOST_DEBUG=1 I can attach a
    ;; debugger and debug a test
    ))

(when mymy-is-not-android
  (use-package lsp-ui
    :ensure t
    :hook
    (lsp-mode . lsp-ui-mode)
    :config
    (setq lsp-ui-sideline-show-hover nil)
    (setq lsp-ui-sideline-delay 0.5)
    (setq lsp-ui-doc-delay 0.5)
    (setq lsp-ui-doc-max-width 80)
    ;; (setq lsp-ui-doc-max-width 150)
    (setq lsp-ui-sideline-ignore-duplicates t)
    (setq lsp-ui-doc-position 'top)
    (setq lsp-ui-doc-alignment 'window)
    (setq lsp-ui-doc-header nil)
    (setq lsp-ui-doc-include-signature t)
    (setq lsp-ui-doc-use-childframe t)))

;; * Haskell
(when mymy-is-not-android
  (use-package haskell-mode
    ;; :ensure (haskell-mode :host github :type git :repo "haskell/haskell-mode")
    :ensure t
    :config
    (define-key haskell-mode-map [f8] 'haskell-navigate-imports)
    (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)
    ;; (define-key interactive-haskell-mode-map (kbd "C-c C-c") 'haskell-compile)
    ;; (gsetq haskell-process-type 'cabal-repl)
    ;; For some reason, when I'm not using auto is not setting the root dir
    ;; of the project. Maybe is due to the fact that the function version
    ;; of this variable doesn't set inferior-haskell-root-dir
    (gsetq haskell-process-type 'auto)
    (custom-set-variables '(haskell-process-type 'cabal-repl))))


;; * Magit
(when mymy-is-not-android
  (use-package magit
    :ensure t))

(when mymy-is-not-android
  (use-package forge
    :after magit
    :ensure t))

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

  (general-define-key
   ;; "C-M-y" 'duplicate-current-line
   ;; "C-S-e" 'forward-word
   ;; "C-q" 'backward-delete-word
   ;; "C-o" 'smart-open-line
   ;; "S-C-<left>" 'shrink-window-horizontally
   ;; "S-C-<right>" 'enlarge-window-horizontally
   ;; "S-C-<down>" 'shrink-window
   ;; "S-C-<up>" 'enlarge-window
   ;; "C-r" 'backward-char
   ;; "C-s" 'forward-char
   ;; "M-r" 'backward-word
   ;; "M-s" 'forward-to-word
   ;; "C-M-s" 'forward-sexp
   ;; "C-M-r" 'backward-sexp
   ;; "C-M-u" 'backward-list
   ;; "M-b" 'isearch-backward
   ;; "M-f" 'isearch-forward
   ;; "C-M-b" 'isearch-backward-regexp
   ;; "C-q" 'backward-delete-word
   ;; "M-z" 'delete-region
   ;; "M-d" 'delete-word-or-whitespace
   ;; "C->" 'mc/mark-next-like-this
   ;; "C-<" 'mc/mark-previous-like-this
   ;; "C-a" 'smarter-move-beginning-of-line
   "M-m" 'new-line-dwim
   ;; "M-e" 'hippie-expand
   ;; "M-n" 'dabbrev-expand
   ;; "C-c s u" 'straight-use-package
   ;; "C-c s g" 'straight-get-recipe
   ;; "C-;" 'iedit-mode
   "C-M-;" 'iedit-mode
   "M-<" #'xref-go-back
   "M->" #'xref-go-forward)

  :config
  (recentf-mode 1)
  ;; With this package I can embark-export consult-line candidates and
  ;; then edit with occur-edit-mode (e). Also, the export buffer
  ;; becomes a occur buffer.
  (general-define-key
   "M-x" 'execute-extended-command
   "C-x b" 'consult-buffer
   "C-x C-f" 'find-file
   "C-c h s" 'consult-line
   "M-g i" 'consult-imenu
   "M-g I" 'consult-imenu-multi)

  ;; Much better than openning a window
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; * Projectile
(use-package projectile
  :demand t
  :ensure t
  ;; TODO: Come here later
  :init
  (setq projectile-keymap-prefix (kbd "C-c k"))

  (defun mymy-search-upwards-with-ripgrep (dir glob-pattern)
    "Search for a file matching GLOB-PATTERN from DIR upwards using ripgrep.
If ripgrep is not available, fall back to `locate-dominating-file`.
Return the directory of the file if found, or nil if not found."
    ;; TODO: Fix this, doesnt work for some reason when used many times
    (if (or t (not (executable-find "rg")))
        (projectile-locate-dominating-file dir glob-pattern)
      (let* ((expanded-dir (expand-file-name dir))
             (dir-to-use (if (file-directory-p expanded-dir)
                             expanded-dir
                           (file-name-directory expanded-dir)))
             (home-dir (expand-file-name "~"))
             (search-command
              (concat "dir=" (shell-quote-argument dir-to-use)
                      "; while [[ $dir != " (shell-quote-argument home-dir) " ]]; do "
                      "rg --files --max-depth 1 -g " (shell-quote-argument glob-pattern)
                      " \"$dir\" --max-count 1 && exit 0; dir=$(dirname \"$dir\"); done"))
             (output (shell-command-to-string search-command)))
        (if (string-empty-p (string-trim output))
            nil
          (file-name-directory (string-trim output))))))

  (defun mymy-projectile-root-csharp (dir)
    "Retrieve the root directory of a C# project in DIR.
This function gives priority to .sln files over .csproj files."
    (let ((root (or (mymy-search-upwards-with-ripgrep dir "*.sln")
                    (mymy-search-upwards-with-ripgrep dir "*.csproj"))))
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
  (gsetq projectile-create-missing-test-files
         t)
  (setq projectile-run-use-comint-mode t)
  (define-key projectile-command-map
              (kbd ".")
              #'projectile-repeat-last-command
              )

  (define-key projectile-mode-map projectile-keymap-prefix 'projectile-command-map)
  ;; (global-set-key projectile-keymap-prefix projectile-command-map)

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
  (add-to-list 'projectile-project-search-path "~/.config/nyxt")
  (add-to-list 'projectile-project-search-path "~/outside_projects/")
  (add-to-list 'projectile-project-search-path "~/personal_projects/")
  ;; (add-to-list 'projectile-ignored-projects
  ;;              (expand-file-name "~"))

  (add-to-list 'projectile-globally-ignored-directories
               "^node_modules$")

  (add-to-list 'projectile-globally-ignored-directories
               (concat "^"
                       (expand-file-name "~/")
                       "$"))
  ;; Will do things like this manually.
  (setq projectile-auto-discover nil)
  ;; Order is impportant
  ;; (add-to-list 'projectile-project-root-functions
  ;;              #'mymy-find-nearest-solution-file t)
  ;; (add-to-list 'projectile-project-root-functions
  ;;              #'not-mymy-find-nearest-chsarp-project t)
  ;; (add-to-list 'projectile-project-root-files
  ;;              "*.sln")

  ;; (benchmark-run 10
  ;;   (projectile-open-projects))

  ;; From https://emacs.stackexchange.com/a/71165
  (defun smart-switch-project ()
    "Open latest edited buffer when switched the  exist project, find files when switched to a new project."
    (if (null (projectile-project-buffer-files))
        (cond
         ((fboundp #'consult-projectile--file)
          (consult-projectile--file (projectile-acquire-root)))
         (t
          (projectile-find-file)))
      (switch-to-buffer (car (projectile-buffers-with-file (projectile-project-buffers))))))

  (setq projectile-switch-project-action 'smart-switch-project)
  (defvar mymy-projectile-marked-projects-file (expand-file-name "marked_projects" user-emacs-directory)
    "File to save the list of marked projects.")
  (defvar mymy-projectile-marked-projects '()
    "List of projects that I consider as working on.")

  (defun mymy-projectile--load-marked-projects ()
    (let ((file mymy-projectile-marked-projects-file))
      (setq mymy-projectile-marked-projects
            (if (file-exists-p file)
                (read (find-file-noselect file))
              (mymy-projectile--save-marked-projects map)
              nil))))

  (defun mymy-projectile--save-marked-projects (&optional data)
    (let ((file mymy-projectile-marked-projects-file)
          (data (or data mymy-projectile-marked-projects)))
      (if (file-writable-p file)
          (with-temp-file file
            (insert (let (print-length) (prin1-to-string data))))
        (message "File '%s' not writeable" filename))))

  (defun mymy-projectile--maybe-load-marked-projects ()
    (let (data)
      (and (null mymy-projectile-marked-projects)
           (setq data (mymy-projectile--load-marked-projects))
           (setq mymy-projectile-marked-projects data))))


  (defun mymy-projectile-mark-project ()
    (interactive)
    (mymy-projectile--maybe-load-marked-projects)
    (when-let (project (projectile-project-root))
      (cl-pushnew project mymy-projectile-marked-projects))
    (mymy-projectile--save-marked-projects))

  (defun mymy-projectile-unmark-project (project)
    (interactive (list (completing-read
                        "Project to unmark: "
                        mymy-projectile-marked-projects)))
    (mymy-projectile--maybe-load-marked-projects)
    (setq mymy-projectile-marked-projects
          (remove project mymy-projectile-marked-projects))
    (mymy-projectile--save-marked-projects))

  (defun mymy-projectile-visit-project ()
    (interactive)
    (mymy-projectile--maybe-load-marked-projects)
    (projectile-switch-project-by-name
     (completing-read
      "Project to visit: "
      mymy-projectile-marked-projects)))

  (defun mymy-projectile-get-nth-project (n)
    "Returns interactive function that switches to nth project"
    (lambda ()
      (interactive)
      (mymy-projectile--maybe-load-marked-projects)
      ;; Reverse, because that is the added other
      (when-let (project (nth n (reverse mymy-projectile-marked-projects)))
        (projectile-switch-project-by-name project))))


  (general-define-key
   :keymaps 'global
   :prefix "C-c h"
   "p" #'mymy-projectile-visit-project
   "a" #'mymy-projectile-mark-project
   "d" #'mymy-projectile-unmark-project
   "0" (mymy-projectile-get-nth-project 0)
   "9" (mymy-projectile-get-nth-project 1)
   "8" (mymy-projectile-get-nth-project 2)
   )

  (general-define-key
   :keymaps 'global
   :prefix "C-c"
   "1" (mymy-projectile-get-nth-project 0)
   "2" (mymy-projectile-get-nth-project 1)
   "3" (mymy-projectile-get-nth-project 2)
   "4" (mymy-projectile-get-nth-project 3)
   )
  :hook
  (elpaca-after-init . projectile-mode))

(use-package consult-projectile
  :after (consult projectile)
  :ensure t
  :init
  ;; Use default action instead of consult find file
  (setq consult-projectile-use-projectile-switch-project t)
  (defvar consult-projectile--open-project-history nil)

  (defvar consult-projectile--source-projectile-open-project
    (list :name "Open Project"
          :narrow '(?o . "Open Project")
          :category 'consult-projectile-project
          :face 'consult-projectile-projects
          :history 'consult-projectile--open-project-history
          :annotate (lambda (dir)
                      (when consult-projectile-display-info
                        (format "Project: %s [%s]"
                                (projectile-project-name dir)
                                (projectile-project-vcs dir))))
          :action (lambda (dir) (funcall consult-projectile-source-projectile-project-action dir))
          :items #'projectile-open-projects))

  (defun consult-projectile-switch-to-open-project ()
    "Jump to open project using `consult'."
    (interactive)
    (funcall-interactively #'consult-projectile '(consult-projectile--source-projectile-open-project)))

  (setq consult-projectile-sources
        '(consult-projectile--source-projectile-buffer
          consult-projectile--source-projectile-file
          consult-projectile--source-projectile-dir
          ;; TODO: Make `projectile-project-root' faster
          ;; consult-projectile--source-projectile-open-project
          ))

  (defvar mymy-projectile-map
    (-doto (make-sparse-keymap)
      (define-key (kbd "p") #'consult-projectile)
      (define-key (kbd "o") #'consult-projectile-switch-to-open-project)
      (define-key (kbd "s") #'consult-projectile-switch-project)
      (define-key (kbd "S") #'projectile-save-project-buffers)
      (define-key (kbd "fb") #'consult-projectile-switch-to-buffer)
      (define-key (kbd "fd") #'consult-projectile-find-dir)
      (define-key (kbd "ff") #'consult-projectile-find-file)
      (define-key (kbd "d") #'consult-projectile-find-dir)
      (define-key (kbd "D") #'projectile-dired)
      (define-key (kbd "i") #'projectile-invalidate-cache)
      (define-key (kbd "k") #'projectile-kill-buffers)
      ;; (define-key (kbd "r") #'projectile-replace)
      ;; (define-key (kbd "R") #'projectile-replace-regexp)
      (define-key (kbd "g") #'consult-ripgrep)
      (define-key (kbd "c") #'projectile-compile-project)
      (define-key (kbd "t") #'projectile-test-project)
      (define-key (kbd "r") #'projectile-run-project)
      (define-key (kbd ".") #'projectile-repeat-last-command)
      ))

  (general-define-key
   :states '(normal motion visual)
   :keymaps 'override
   :prefix "SPC"
   "p" (list mymy-projectile-map :which-key "Project"))
  :config
  ;; Use consult--bufer-query
  (comment
   (el-patch-defvar consult-projectile--source-projectile-buffer
     (list :name     "Project Buffer"
           :narrow   '(?b . "Buffer")
           :category 'buffer
           :face     'consult-buffer
           :history  'buffer-name-history
           :state    #'consult--buffer-state
           :enabled  #'projectile-project-root
           :items
           (lambda ()
             (when-let (root (projectile-project-root))
               (mapcar #'buffer-name
                       (el-patch-swap
                         (seq-filter (lambda (x)
                                       (when-let (file (buffer-file-name x))
                                         (string-prefix-p root file)))
                                     (consult--buffer-query :sort 'visibility))
                         (consult--buffer-query
                          :sort 'visibility
                          :directory root
                          ;; :predicate (lambda (x)
                          ;;              (when-let (file (buffer-file-name x))
                          ;;                (string-prefix-p root file)))
                          )
                         )
                       ))))))
  )


(comment
 (defconst mymy-frecency-file
   (expand-file-name "vertico-frecency-data.el" user-emacs-directory)
   "File to store frecency data.")

 (defun mymy-get-frecency-data ()
   "Retrieve frecency data from file."
   (if (file-exists-p mymy-frecency-file)
       (with-temp-buffer
         (insert-file-contents mymy-frecency-file)
         (condition-case nil
             (read (buffer-string))
           (error nil)))
     nil))

 (defun mymy-save-frecency-data (data)
   "Save frecency DATA to file."
   (with-temp-file mymy-frecency-file
     (let ((print-length nil)
           (print-level nil))
       (prin1 data (current-buffer)))))

 (use-package frecency
   :ensure t
   :config
   (with-eval-after-load 'vertico
     (defun mymy-vertico-frecency-sort (candidates)
       (let* ((frecency-data (mymy-get-frecency-data))
              (candidates-with-frecency
               (mapcar (lambda (candidate)
                         (cons candidate (plist-get frecency-data candidate)))
                       candidates))
              (sorted (frecency-sort candidates-with-frecency
                                     :get-fn (lambda (item key)
                                               (plist-get (cdr item) key)))))
         (mapcar #'car sorted)))

     (defun mymy-update-frecency-data (&rest args)
       (message "%S" args)
       (let* ((candidate (vertico--candidate))
              (frecency-data (or (mymy-get-frecency-data) '()))
              (candidate-data (or (plist-get frecency-data candidate) '())))
         (setq frecency-data
               (plist-put frecency-data candidate
                          (frecency-update candidate-data
                                           :get-fn #'plist-get
                                           :set-fn #'plist-put)))
         (mymy-save-frecency-data frecency-data)))

     (advice-add 'vertico-insert :after #'mymy-update-frecency-data)
     (setq vertico-sort-function #'mymy-vertico-frecency-sort))))
;; * Vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  (setq enable-recursive-minibuffers t))

(when mymy-is-not-android
  (use-package vertico-posframe
    :disabled
    :ensure t
    :config
    (require 'vertico-multiform)
    (add-to-list 'vertico-multiform-commands
                 '(t posframe)
                 )
    (vertico-multiform-mode 1)))

;; * Marginalia
(use-package marginalia
  :ensure t
  :config
  ;; Until I find the way.
;;; I don't remember why I said the previous thing
  (marginalia-mode))

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
   "M-<return>" 'mymy-act-at-point
   "M-RET" 'mymy-act-at-point
   )
  ;; (setq embark-verbose-indicator-display-action '(display-buffer-reuse-window))
  )

(use-package embark-consult
  ;; :no-require t
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :ensure t)

;; * Orderless
(use-package orderless
  :ensure t
  :config
  ;; ;; Put orderless at last since orderless put me things almost at random.
  ;; ;; (add-to-list 'completion-styles 'orderless t)
  ;; ;; (setq completion-styles '(basic partial-completion orderless))
  ;; (setq completion-styles '(basic partial-completion substring orderless))
  ;; (setq completion-styles '(basic orderless))
  ;; (setq completion-styles '(orderless))
  (setq completion-styles '(orderless basic))
  ;; ;; matching characters in order, but non-consecutively
  ;; ;; (add-to-list 'orderless-matching-styles 'orderless-flex t)
  ;; (setq orderless-matching-styles '(;; orderless-literal
  ;;                                   orderless-regexp orderless-prefixes))
  (setq orderless-matching-styles '(orderless-literal
                                    orderless-regexp
                                    ;; orderless-flex
                                    ))
  (setq completion-category-overrides '((file (styles basic partial-completion substring))))
  ;; (setq completion-category-overrides '((file (styles basic substring))))
  )

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
  ;; :ensure (corfu :files ("*.el" "extensions/*.el"))
  :ensure t
  ;; Optional customizations
  :config
  (gsetq corfu-cycle t)      ;; Enable cycling for `corfu-next/previous'
  (gsetq corfu-auto t)       ;; Enable auto completion
  ;; (gsetq corfu-separator ?\s) ;; Orderless field separator
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

   (advice-add #'corfu--post-command :around #'force-debug))

  :init
  (general-define-key
   "C-M-e" 'completion-at-point
   ;;  "M-/" 'dabbrev-completion
   ;;  "C-M-/" ' dabbrev-expand
   )

  (general-define-key
   :keymaps 'corfu-map
   ;; Default: M-h
   "M-h" 'corfu-info-documentation
   ;; Default: M-g
   "M-g" 'corfu-info-location
   ;; Default: completion-at-point, TAB
   [completion-at-point] 'corfu-complete
   "TAB" 'corfu-complete
   ;; "SPC" 'corfu-insert-separator
   "RET" 'corfu-complete)

  (global-corfu-mode)
  (corfu-echo-mode)
;;; Like company quickhelp, except that opens the frame
  ;; (corfu-popupinfo-mode -1)
  )

(use-package corfu-terminal
  :ensure t
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1))
  )

(use-package cape
  :ensure t
  :after (corfu)
  :config
  (general-define-key
   "C-M-k" 'cape-file))

(use-package nerd-icons-corfu
  :ensure (:host github :type git :repo "LuigiPiucco/nerd-icons-corfu")
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; * Nerd icons
(use-package nerd-icons
  :ensure t)

;; * sxhkdrc-mode

(use-package sxhkdrc-mode
  :ensure t
  :init
  (add-to-list
   'auto-mode-alist
   '("swhkdrc\\'" . sxhkdrc-mode)
   )
  )

;; * Yuck mode

(use-package yuck-mode
  :ensure )

;; * Terminal here
;; Love this, just a open-system terminal here
(when mymy-is-not-android
  (use-package terminal-here
    :ensure t
    :config
    (global-set-key (kbd "C-<f5>") #'terminal-here-launch)
    (global-set-key (kbd "C-<f6>") #'terminal-here-project-launch)
    (setq terminal-here-linux-terminal-command (if (string= "tic12" (system-name))
                                                   ;; Run windows terminal (wt) and then run wsl
                                                   '("wt.exe" "wsl")
                                                 '("kitty" "--single-instance")))
    (setq terminal-here-command-flag "--")
    ;; (when (executable-find "poetry")
    ;;   (global-set-key (kbd "C-<f3>") (lambda () (interactive) (terminal-here-launch (list (executable-find "poetry") "shell")))))

    (defun mymy-set-xmonad-project-dir-here ()
      (interactive)
      (and (y-or-n-p "Want to change the xmonad directory?")
           (when (not (= 0 (shell-command (format (expand-file-name "~/Scripts/xmonadctl -a XMONAD_CHANGE_DIR %S") default-directory))))
             (error "Error setting the xmonad dir"))))
    (global-set-key (kbd "C-<f4>") #'mymy-set-xmonad-project-dir-here)))

;; * Undo tree
(use-package undo-tree
  :ensure t
  :init
  (add-hook 'elpaca-after-init-hook
            #'global-undo-tree-mode)
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
        ("<remap> <next-line>" . widget-forward)
        ("<remap> <right-char>" . widget-forward)
        ("<remap> <left-char>" . widget-backward))
  :config
  (defun doom-dashboard-insert-quick-access-shortmenu (&rest _)
    "Insert quick access shortmenu widget."
    (let* ((fn (alist-get 'quick-access doom-dashboard-shortmenu-functions))
           (fn-keymap (format "\\[%s]" fn))
           (icon-name (alist-get 'bookmarks dashboard-heading-icons))
           (icon (nerd-icons-octicon icon-name :face 'dashboard-heading)))
      (if dashboard-display-icons-p
          (insert (string-pad icon 3)))
      (widget-create 'item
                     :tag (format "%-30s" "Open quick access menu")
                     :action (lambda (&rest _)
                               (call-interactively
                                (alist-get 'quick-access doom-dashboard-shortmenu-functions)))
                     :mouse-face 'highlight
                     :button-face 'dashboard-heading
                     :button-prefix ""
                     :button-suffix ""
                     :format "%[%t%]")
      (if doom-dashboard-set-widget-binding
          (insert (propertize (substitute-command-keys fn-keymap)
                              'face
                              'doom-dashboard-bindings-face)))))
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
         `((recents . recentf)
           (quick-access . mymy-quick-access)
           (bookmarks . bookmark-jump)
           (projects . consult-projectile-switch-project)
           (agenda . org-agenda)))
  (gsetq dashboard-item-generators
         '((recents . doom-dashboard-insert-recents-shortmenu)
           (quick-access . doom-dashboard-insert-quick-access-shortmenu)
           (bookmarks . doom-dashboard-insert-bookmark-shortmenu)
           (projects . doom-dashboard-insert-project-shortmenu)
           (agenda . doom-dashboard-insert-org-agenda-shortmenu)))
  (gsetq dashboard-items '(projects agenda quick-access recents)))

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
    ("a" dirvish-quick-access)
    ("f" dirvish-file-info-menu)
    ("y" dirvish-yank-menu)
    ("N" dirvish-narrow)
    ("^" dirvish-history-last)
    ("h" dirvish-history-jump)       ; remapped `)describe-mode'
    ("s" dirvish-quicksort)          ; remapped `dired-sort-toggle-or-edit'
    ("v" dirvish-vc-menu)            ; remapped `dired-view-file'
    ("TAB" dirvish-subtree-toggle)
    ("M-f" dirvish-history-go-forward)
    ("M-b" dirvish-history-go-backward)
    ("M-l" dirvish-ls-switches-menu)
    ("M-m" dirvish-mark-menu)
    ("M-t" dirvish-layout-toggle)
    ("M-s" dirvish-setup-menu)
    ("M-e" dirvish-emerge-menu)
    ("M-j" dirvish-fd-jump)
    ("." nil))
  :bind
  ((:map dirvish-mode-map
         ("a" . dirvish-quick-access)
         ("f" . dirvish-file-info-menu)
         ("y" . dirvish-yank-menu)
         ("N" . dirvish-narrow)
         ("^" . dirvish-history-last)
         ("h" . dirvish-history-jump)   ; remapped `describe-mode'
         ("s" . dirvish-quicksort)   ; remapped `dired-sort-toggle-or-edit'
         ("v" . dirvish-vc-menu)     ; remapped `dired-view-file'
         ("TAB" . dirvish-subtree-toggle)
         ("C-<tab>" . dirvish-subtree-toggle)
         ("M-f" . dirvish-history-go-forward)
         ("M-b" . dirvish-history-go-backward)
         ("M-l" . dirvish-ls-switches-menu)
         ("M-m" . dirvish-mark-menu)
         ("M-t" . dirvish-layout-toggle)
         ("M-s" . dirvish-setup-menu)
         ("M-e" . dirvish-emerge-menu)
         ("M-j" . dirvish-fd-jump))
   (:map dired-mode-map (("C-l" . dired-up-directory)
                         ("." . hydra-dirvish/body ;; dirvish-dispatch
                          )))))

;; * Yasnippet
(use-package yasnippet ;; Only for lsp-mode
  :ensure t
  :config
  (add-hook
   'elpaca-after-init-hook
   'yas-global-mode))

;; * Tempel
(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")
  :ensure t
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ;; ("C-c e" . tempel-complete)
         ("M-*" . tempel-insert))

  :init
  (gsetq tempel-trigger-prefix "<")
  (setq mymy-template-files (expand-file-name
                             "tempel/"
                             dropbox-dir))
  (setq tempel-path (list (concat mymy-template-files "*.eld")
                          (concat mymy-template-files "*/*.eld")
                          ;; Only works because `tempel-auto-reload' is true
                          "local-tempel.eld"))

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
      ((lang "arsdasrt"))))
  (defun mymy-tempel-bind (elt)
    "Introduces the element 'b', it includes other templates by name
and also binds their names fields with lisp expressions (or values).
It is essentially the element include but with args."
    (when (eq (car-safe elt) 'b)
      (if-let (template (alist-get (cadr elt) (tempel--templates)))
          (cons 'l (dolist (caddr elt) template))
        (message "Template %s not found" (cadr elt))
        nil)))

  (add-to-list 'tempel-user-elements #'tempel-include)

  (defun mymy-tempel-add-org-babel-languages ()
    "Load all snippets to tempel"
    (when (string-equal major-mode "org-mode")
      (->> org-babel-load-languages
           (-filter 'cdr)
           (-map 'car)
           (--map `(,(intern-soft it)
                    ,(concat "#+begin_src " (symbol-name it))
                    n q n r n> "#+end_src")))))

  ;; This thing is used in template
  (add-to-list 'tempel-template-sources #'mymy-tempel-add-org-babel-languages))

;; * Org mode
(use-package doct
  ;; Description: doct is a function that provides an alternative,
  ;; declarative syntax for describing Org capture templates.
  :ensure t)

(use-package org-contrib
  :ensure t)


(use-package org
  :after doct org-contrib
  :init
  ;; See C-c C-u, C-c C-b, C-c C-p, C-c C-n
  ;; (with-eval-after-load 'evil
  ;;   (with-eval-after-load 'general
  ;;     (general-define-key
  ;;      :keymaps 'outline-mode-map
  ;;      :state '(normal visual motion)
  ;;      "gb" 'org-back-to-heading
  ;;      "gB" 'mymy-org-back-to-heading
  ;;      )
  ;;     )
  ;;   )
  ;; (setq org-export-publishing-directory "./artifacts")
  (gsetq org-file-apps
         '((auto-mode . emacs)
           (directory (direction . top)
                      . emacs)
           ("\\.mm\\'" . default)
           ("\\.x?html?\\'" . default)
           ("\\.pdf\\'" . "zathura %s")))

  (gsetq org-directory (expand-file-name "text/" mymy-organization-system-directory))

  (setq mymy-org-gtd-file "20240928T212555--gtd.org")
  (setq mymy-org-inbox-file "20240929T081115--inbox.org")
  (setq mymy-org-projects-file "20240112T082246--projects.org")
  (setq mymy-org-done-file "20240910T103629--done.org")
  (setq mymy-org-school-file "20240117T132013--school.org")
  (setq org-agenda-file-regexp "\\`[^.][0-9]*T[0-9]*--.*\\.org\\'")
  ;; (setq org-agenda-file-regexp "\\`[^.].*\\.org\\'")

  (gsetq org-agenda-files
         (list org-directory)
         ;; (list mymy-org-gtd-file
         ;;       mymy-org-inbox-file
         ;;       mymy-org-projects-file
         ;;       mymy-org-school-file
         ;;       "mobile/2024-01-01_mobile_inbox.org")
         )

  (gsetq org-refile-targets
         '((nil :maxlevel . 3)
           (mymy-org-done-file :maxlevel . 1)
           ("20240902T174614--school-done.org" :maxlevel . 1)
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
                 (direction . top)
                 (window-parameters
                  (mode-line-format .
                                    (:eval
                                     (mymy-mode-line-word-with-padding
                                      "Agenda"))))))

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
                        "- What to do"
                        ""
                        "  "
                        ""
                        "- Why do it"
                        ""
                        " "
                        ))
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
            ;; I don't have that many, and have reached a workflow where I
            ;; manually create this things in the tasks.
            ;; ("Homework" :keys "h"
            ;;  :type entry
            ;;  :file ,mymy-org-school-file
            ;;  :headline "Inbox"
            ;;  :template ("* TODO %? [/]"
            ;;             ":PROPERTIES:"
            ;;             ":CREATED: %<%Y-%m-%d-%H-%M-%S>"
            ;;             ":END:"
            ;;             ""
            ;;             "DEADLINE: %^{Deadline}T"
            ;;             "SCHEDULED: %^{Scheduled}t"
            ;;             ""
            ;;             "- Tasks"
            ;;             "  - [ ] "
            ;;             ""
            ;;             "- Assignment"
            ;;             ""
            ;;             "  "
            ;;             )
            ;;  )

            ;; I have achived another workflow where all of this things are
            ;; worked out after creating the node. So the note is just the
            ;; content, other notes can surround it and append the other
            ;; elements.
            ;; ("Note" :keys "n"
            ;;  :type entry
            ;;  :file ,mymy-org-inbox-file
            ;;  :headline "Notes"
            ;;  :template ("* %? :notes:"
            ;;             ":PROPERTIES:"
            ;;             ":CREATED: %<%Y-%m-%d-%H-%M-%S>"
            ;;             ":END:"
            ;;             ""
            ;;             "- Elements: Content, Concept, Composition. Main idea, Examples, Related"))
            )))

  ;; ;; Org define keys (:prefix C-c o)
  ;; (general-define-key
  ;;  :prefix "C-c o"
  ;;  "c" 'org-capture
  ;;  "w" 'hydra-org-web-tools/body
  ;;  "a" 'org-agenda
  ;;  "t" 'mymy-org-clock-toggle
  ;;  "s" 'my/org-agenda-rest
  ;;  )
  (general-define-key
   "<f12>" 'org-agenda
   "C-c a" 'org-agenda
   "C-c c" 'org-capture
   "C-c s" 'my/org-agenda-rest
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
                              '(("^\\*+ \\(?:.*[        ]\\)?\\(:\\([[:alnum:]_@#%:]+\\):\\)[   ]*$"
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
     (js . t)
     ;; (mysql . t)
     (sqlite . t)
     (clojure . t)
     (java . t)
     (csharp . t)
     (lisp . t)
     (shell . t)
     ;; (restclient . t)
     (haskell . t)
     ))
  :config
  (defcustom mymy-org-run-commands '(("drg" . "/usr/bin/dragon-drop %n")
                                     ("pdf" . "/usr/bin/zathura %n")
                                     )
    "List of alist of the form (NAME . COMMAND) where COMMAND is template that takes:
%n for the name of the file.

COMMAND will be run asynchronously")

  (defun mymy-org-run-command-on-current-pdf (&optional arg)
    (interactive "P")
    (let ((target (expand-file-name (org-export-output-file-name ".pdf" (null arg)))))
      (unless (file-exists-p target)
        (org-latex-export-to-pdf nil t))
      (when-let (process (alist-get
                          (completing-read "Command to run: "
                                           mymy-org-run-commands
                                           nil t)
                          mymy-org-run-commands
                          nil nil #'equal)
                         )
        (let ((display-buffer-alist (cons (list shell-command-buffer-name-async
                                                '(display-buffer-no-window))
                                          display-buffer-alist)))
          (async-shell-command
           (format-spec
            process
            `((?n . ,(shell-quote-argument target)))))))))

  (with-eval-after-load 'general
    (general-define-key
     :keymaps 'org-mode-map
     "C-c v" #'mymy-org-run-command-on-current-pdf
     )
    )

  (org-link-set-parameters
   "attach"
   :follow (lambda (filename)
             (thread-last filename
                          (concat mymy-organization-system-directory-attachments)
                          find-file))
   :help-echo "Filename in the `mymy-organization-system-directory-attachments' directory"
   :face '(:foreground "DarkSeaGreen" :underline t)
   :export (lambda (path desc format)
             (let ((full-path (concat mymy-organization-system-directory-attachments path)))
               (org-export-string-as
                (if desc
                    (format "[[%s][%s]]" full-path desc)
                  (format "[[%s]]" full-path))
                format t)))
   )

  (defconst mymy-org-attach-prefix "attach:"
    "The part of the text in org mode that starts a link")

  (with-eval-after-load 'cape
    ;; Basically stolen from cape
    (defun mymy-org-attach-complete-at-point ()
      "Complete file name for attach:' links in Org mode."
      (pcase-let* ((default-directory mymy-organization-system-directory-attachments)
                   (prefix (and (looking-back (concat mymy-org-attach-prefix "[^]]*")
                                              (line-beginning-position))
                                (match-beginning 0)))
                   (`(,beg . ,end) (if prefix
                                       (cons (+ prefix (length mymy-org-attach-prefix)) (point))
                                     (cape--bounds 'filename)))
                   (non-essential t)
                   (file (buffer-substring-no-properties beg end)))
        (when prefix
          `(,beg ,end
                 ,(cape--nonessential-table #'read-file-name-internal)
                 :company-prefix-length t
                 :exclusive no
                 :annotation-function
                 ,(lambda (cand)
                    (let ((type (if (file-directory-p (expand-file-name cand default-directory))
                                    "Dir" "File")))
                      (format " (%s)" type)))
                 :company-docsig
                 ,(lambda (cand)
                    (let ((full-path (expand-file-name cand default-directory)))
                      (format "%s (%s)" full-path (file-size-human-readable (file-attribute-size (file-attributes full-path))))))))))

    (org-link-set-parameters
     "attach"
     :complete #'mymy-org-attach-complete-at-point)

    (defun mymy-org-attach-setup ()
      "Set up completion-at-point and link parameters for `attach:' links."
      (add-hook 'completion-at-point-functions #'mymy-org-attach-complete-at-point nil t))

    (add-hook 'org-mode-hook #'mymy-org-attach-setup)
    )

  ;; Load after corfu, could be any completion framework
  (with-eval-after-load 'corfu
    ;; Use org-goto via completion (the other option is to use the buffer)
    (setq org-goto-interface 'outline-path-completion)
    (setq org-outline-path-complete-in-steps nil)
    )

  (require 'ox-extra)
  ;; Add the option of ignoring headline while including content with :ignore: tag
  (ox-extras-activate '(ignore-headlines))

  ;; ** Setup for exporting to a directory using org-export

  (defcustom mymy-org-default-export-directory (and nil "artifacts")
    "If publishing is directory is not set. Use this directory. If this is
     nil, then use the current directory.")

  (defun mymy-org-export-output-file-name-advice-filter-args (&rest args)
    (let ((args (car args)))
      (let ((extension (nth 0 args))
            (subtreep (nth 1 args))
            (pub-dir (nth 2 args)))
        (when (and (not pub-dir)
                   mymy-org-default-export-directory
                   (not (file-exists-p mymy-org-default-export-directory)))
          (make-directory mymy-org-default-export-directory t))
        (list
         extension
         subtreep
         (or pub-dir
             mymy-org-default-export-directory)))))

  (advice-add
   #'org-export-output-file-name
   :filter-args
   #'mymy-org-export-output-file-name-advice-filter-args)

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
      (org-agenda nil "nn")
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

  (setq org-latex-compiler "pdflatex")
  (setq org-latex-pdf-process
        ;; -pdfxe: use xelatex -pdflua: use luatex -bibtex use bibtex when needed
        ;; -xelatex use xelatex for processing files to pdf and turn dvi/ps modes off
        ;; -f: Force -pdf output pdf -bibtex
        ;; (list "latexmk -bibtex -f -pdf %f")
        ;; (list "latexmk -f -pdf -shell-escape -%latex -interaction=nonstopmode -output-directory=%o -aux-directory=%o %f")
        ;; (list "latexmk -f -pdf -shell-escape -xelatex -interaction=nonstopmode -output-directory=%o %f")
        (list (string-join
               (list
                ;; The best joke in the group
                "cd" "%o"
                "&&"
                "latexmk"
                "-f"
                "-pdf"
                "-shell-escape"
                "-%latex"
                "-interaction=nonstopmode"
                "-output-directory=%o"
                "-aux-directory=%o"
                ;; "-usepretex='\\PassOptionsToPackage{outputdir=%o}{minted}'"
                "%f"
                )
               " "))
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
          ("KILL" . (:foreground "red" :weight bold))
          ("SOMEDAY" . (:foreground "#F09432" :weight italics))
          ("OTHER" . (:foreground "yellow" :weight bold))
          ;; PROJect, as in something without a clear goal
          ;; ("PROJ" . (:foreground "white" :weight bold))
          ))

  (setq org-todo-keywords
        '((sequence "TODO(t@/!)" "NEXT(n@/!)" "OTHER(h@/!)" "|" "DONE(s@/!)")
          (type "KILL(k@/!)" "SOMEDAY(o@/)")
          ;; (type "PROJ(p)")
          ))

  (setq org-default-notes-file (f-join mymy-organization-system-directory-text "agenda.org"))
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
  (org-mode . (lambda () (setq-local indent-tabs-mode nil
                                     python-shell-interpreter "python3"))))

(when (and mymy-is-not-android mymy-we-are-not-at-work)
  (use-package emacsql
    :ensure t
    :config
    (emacsql-fix-vector-indentation)
    )
  (use-package org-roam
    :after (emacsql)
    :ensure t
    :init
    (setq org-roam-directory (expand-file-name
                              mymy-organization-system-directory-text))
    (setq org-roam-dailies-directory "daily/")
    (setq org-roam-capture-templates
          '(("d" "default" plain "%?"
             ;; Denote timestamp
             :target (file+head "%<%Y%m%dT%H%M%S>--${slug}.org"
                                "#+title: ${title}\n")
             :unnarrowed t))
          )
    :config
    (general-define-key
     :keymap 'global
     :prefix "C-c n"

     "l" 'org-roam-buffer-toggle
     "f" 'org-roam-node-find
     "g" 'org-roam-graph
     "i" 'org-roam-node-insert
     "c" 'org-roam-capture
     "j" 'org-roam-dailies-capture-today)
    ;; If you're using a vertical completion framework, you might want a more informative completion interface
    (setq org-roam-node-display-template
          (concat "${title:*} "
                  (propertize "${tags:10}" 'face 'org-tag)))
    (org-roam-db-autosync-mode)))

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
  (setq org-agenda-skip-function-global
        '(org-agenda-skip-entry-if 'todo '("KILL"))
        )
  (general-define-key
   :states '(normal motion visual)
   :keymaps 'org-agenda-mode-map
   "C-<tab>" 'org-agenda-show-and-scroll-up
   )

  (setq mymy-org-agenda-tags-width 0)
  (defconst mymy-org-agenda-custom-commands-file
    (expand-file-name
     "agenda-views.eld"
     mymy-organization-system-directory-text
     )
    )

  (defvar mymy-org-agenda-custom-commands-timestamp nil
    "Timestamp for `org-agenda-custom-commands'")

  (defun mymy-org-agenda-load-file (file)
    ;; Taken from tempel
    (with-temp-buffer
      (insert "(\n")
      (insert-file-contents file)
      (goto-char (point-max))
      (insert "\n)")
      (goto-char (point-min))
      (read (current-buffer))))

  (defun mymy-org-agenda-reload-file (file old-timestamp)
    (let ((new-timestamp
           (time-convert
            (file-attribute-modification-time
             (file-attributes (file-truename file)))
            'integer)))
      (unless (and
               old-timestamp
               (equal
                old-timestamp
                new-timestamp))
        `(,new-timestamp . ,(mymy-org-agenda-load-file file)))))

  (defun mymy-org-agenda-commands-maybe-reload (&rest ignore)
    (when-let ((result (mymy-org-agenda-reload-file
                        mymy-org-agenda-custom-commands-file
                        mymy-org-agenda-custom-commands-timestamp)))
      (setq
       mymy-org-agenda-custom-commands-timestamp
       (car result))
      (setq
       org-agenda-custom-commands
       (cdr result))
      result))

  (advice-add #'org-agenda :before #'mymy-org-agenda-commands-maybe-reload)

  ;; Modified what is necessary
  ;; https://www.reddit.com/r/emacs/comments/yfqq6g/comment/iujgmh3/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
  ;; Why is the name so weird?
  (defun my/org-agenda-rest (&optional arg)
    "Prompt to select a custom agenda view and display the agenda,
bypassing the dispatch buffer."
    (interactive "P")
    (when arg (org-check-for-org-mode))
    (let* ((views
            (cl-loop for (key . body) in org-agenda-custom-commands
                     ;; Skip leader keys, which have no body
                     when (consp body)
                     collect (cons (car body) key)
                     ))
           (view (alist-get (completing-read "Select an agenda view: " views) views nil nil #'equal))
           (restr (pcase current-prefix-arg
                    ('(4) 'buffer)
                    ('(16) 'subtree)
                    (_ nil))))
      (org-agenda nil view restr)))

  ;; (setq org-agenda-custom-commands
  ;;       (mymy-org-agenda-load-file mymy-org-agenda-custom-commands-file)
  ;;       )

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
        nil
        ;; '((:name "At Phone"
        ;;          :and (:todo "TODO" :tag "@phone"))
        ;;   (:name "Homework"
        ;;          :and (:todo ("TODO" "NEXT") :tag "school"))
        ;;   (:name "Pinned to do now"
        ;;          :and (:todo "NEXT" :scheduled t))
        ;;   (:name "Schedule of the day"
        ;;          :and (:todo "TODO" :scheduled t))
        ;;   ;; :auto-planning
        ;;   (:name "Done" :todo "DONE")
        ;;   ;; (:discard (:todo "DONE"))
        ;;   (:name "Stuck" :anything)
        ;;   ;; (:discard (:anything t))
        ;;   )
        )
  :config
  (general-define-key
   :keymaps 'org-super-agenda-header-map
   "j" 'org-agenda-next-line
   "k" 'org-agenda-previous-line
   )
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
                              it)))))))

(use-package org-evil
  :ensure nil
  :no-require t
  :config
  (general-define-key
   :states '(normal motion)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-c"
   "o p" #'org-pomodoro
   "o c" #'org-capture
   "o a" #'org-agenda
   "o i" #'org-clock-in
   "o o" #'org-clock-out
   "o g" #'org-clock-goto
   "o t" #'mymy-org-clock-toggle
   "o l" #'org-toggle-link-display
   ;; "o w" #'hydra-org-web-tools/body
   "o s" #'my/org-agenda-rest
   ))

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

(use-package ox-moderncv
  :ensure (ox-moderncv :host gitlab
                       :repo "Titan-C/org-cv")
  :config
  (defun mymy-ox-moderncv-export-this-file ()
    (interactive)
    (org-export-to-file 'moderncv "moderncv.tex")
    (org-latex-compile "moderncv.tex")))

;; * Text mode utils
;; Put a more comfortable fill-column
(setq-default fill-column 75)

;; Run olivetti, adaptative-wrap and visual-line mode to
(add-hook 'org-mode #'visual-line-mode)

(use-package adaptive-wrap
  :ensure t
  :hook (org-mode . adaptive-wrap-prefix-mode)
  :init
  (setq adaptive-wrap-extra-indent 0))

(use-package olivetti
  :ensure t
  :hook ((olivetti-mode . mymy-configure-olivetti)
         (org-mode . olivetti-mode))
  :init
  (setq fringes-outside-margins t)
  (custom-set-faces
   '(olivetti-fringe ((t (:foreground "#353535" :background "#353535")))))
  (gsetq olivetti-style 'fancy)
  (defun mymy-configure-olivetti ()
    (interactive)
    (setq olivetti-body-width (+ 4 fill-column))))

(use-package stripes
  :ensure t
  :config
  (setq stripes-unit 1)
  )

;; * Flycheck
(use-package flycheck
  :ensure t
  :config
  (gsetq flycheck-indication-mode 'right-fringe)
  ;; Use M-g M-n or M-g M-p
  ;; (general-define-key
  ;;  :keymaps 'flycheck-mode-map
  ;;  "M-n" 'flycheck-next-error
  ;;  "M-p" 'flycheck-previous-error
  ;;  )

  (defun mymy-flycheck-error-list-mode-hook ()
    (visual-line-mode 1)
    (with-eval-after-load 'stripes
      (stripes-mode 1)
      )
    )

  (add-hook
   'flycheck-error-list-mode-hook
   #'mymy-flycheck-error-list-mode-hook
   )

  ;; (add-to-list 'display-buffer-alist
  ;;              '((major-mode . flycheck-error-list-mode)
  ;;                (display-buffer-in-side-window)
  ;;                (window-height . 0.30)
  ;;                (window-width . 0.55)
  ;;                (dedicated . t)
  ;;                (side . bottom)
  ;;                (slot . 0)
  ;;                (window-parameters . ((no-other-window . t)
  ;;                                      (no-delete-other-windows . t)
  ;;                                      (mode-line-format . 'none)))))
  )

;;* Denote
(use-package denote
  :defer 5
  :ensure t
  :hook (;; Note: Only works on files that have an identifier on their
         ;; filename
         ;; (find-file . denote-link-buttonize-buffer)
         (dired-mode . denote-dired-mode))
  :init
  ;; Keep it in parallel to main system of orgnization
  (setq denote-directory (concat dropbox-dir "notes/"))
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-project-search-path
                 denote-directory))
  ;; Really easy to upgrade to Markdown and then org mode
  (setq denote-file-type 'text)
  ;; (setq denote-link-button-action #'mymy-denote-link-button-action)
  ;; Let's first try the default action
  (setq denote-open-link-function #'find-file-other-window)

  (defvar mymy-denote-mark-ring nil
    "Mark for position before link jumping in denote.")

  (defvar mymy-denote-mark-ring-last-goto nil
    "Last position in the mark ring used to go back.")

  (defcustom mymy-denote-length-mark-ring 200
    "The length of the mark ring"
    :set (lambda (var value)
           (setq mymy-denote-length-mark-ring value)
           (dotimes (_ mymy-denote-length-mark-ring)
             (push (make-marker) mymy-denote-mark-ring))
           (setcdr (nthcdr (1- mymy-denote-length-mark-ring) mymy-denote-mark-ring)
                   mymy-denote-mark-ring)))

  (defun mymy-denote-pop-mark-ring (&optional n)
    "Pop and go to the previous N position of mark ring. If N is not set
then go back 1."
    (interactive "p")
    (let (p m)
      (if (eq last-command this-command)
	  (setq p (nthcdr n (or mymy-denote-mark-ring-last-goto mymy-denote-mark-ring)))
        (setq p mymy-denote-mark-ring))
      (setq mymy-denote-mark-ring-last-goto p)
      (setq m (car p))
      (pop-to-buffer-same-window (marker-buffer m))
      (goto-char m)))

  (defun mymy-denote-mark-ring-push ()
    (let ((pos (point))
          (buffer (current-buffer)))
      (with-current-buffer buffer
        (org-with-point-at pos (push-mark nil t)))
      (setq mymy-denote-mark-ring
            (nthcdr (1- mymy-denote-length-mark-ring) mymy-denote-mark-ring))
      (move-marker (car mymy-denote-mark-ring) pos buffer)))

  (defun mymy-denote-link-button-action (path)
    "Open denote link and push current position to mark ring."
    (interactive)
    (mymy-denote-mark-ring-push)
    (find-file path)
    )

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

  (defun mymy-denote-find-link-at-point ()
    (interactive)
    ;; The same as `denote-link-return-links' but with user-error
    (save-excursion
      (let ((case-fold-search nil)
            (start (point)))
        ;; Find the next closing bracket(s) after point
        (when (re-search-forward "\\]+" (line-end-position) t)
          ;; Now search backward for the complete [[denote:<id>]] pattern
          (when (re-search-backward "\\[\\[denote:\\([^]]+\\)\\]\\]" (line-beginning-position) t)
            ;; Verify that point was originally within this link
            (when (and (>= start (match-beginning 0)) (<= start (match-end 0)))
              (if-let ((id (match-string 1))
                       (path (denote-get-path-by-id id)))
                  (funcall denote-open-link-function path)
                (user-error "Cannot resolve the denote link at point")))))
        (user-error "No denote link found at point"))))
  (defun mymy-denote-copy-current-as-link ()
    (interactive)
    (let ((filename (f-base (buffer-file-name))))
      (when (length> filename 15)
        (kill-new (concat
                   "[[denote:"
                   (substring filename 0 15)
                   "]]"))
        )
      )
    )

  


  :config

  (general-define-key
   :keymap 'text-mode-map
   "TAB" 'indent-according-to-mode)

  (general-define-key
   :prefix "C-c m"

   "n" #'denote
   ;; Call with C-u to insert without description
   "i" #'mymy-denote-link
   "e" '(ignore :which-key "Open for posibitlies")
   "o" #'denote-open-or-create

   "q" #'mymy-denote-pop-mark-ring
   "f" #'mymy-denote-find-file
   "g" '((lambda () (interactive) (find-file denote-directory)) :which-key "Go to denote dir")
   "w" '((lambda () (interactive) (consult-ripgrep denote-directory)) :which-key "Grep in denote dir")
   "l" #'denote-find-link
   "." #'mymy-denote-find-link-at-point
   "k" #'mymy-denote-copy-current-as-link

   "r" #'denote-rename-file
   "R" #'denote-rename-file-using-front-matter
   )

  ;; (global-set-key (kbd "C-c m") mymy-denote-map)
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
  (keymap-set hl-todo-mode-map "C-c o o" #'consult-todo))

(when mymy-is-not-android
  (use-package magit-todos
    :ensure t
    :after magit
    :config
    (add-to-list 'magit-todos-exclude-globs
                 ;; Exclude dotnet MVC wwwroot libraries
                 "**/wwwroot/lib/")
    (magit-todos-mode 1)))

;; * Csharp
(when mymy-is-not-android
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
      '("w" "new" mymy-sharper-transient-new))))

(when mymy-is-not-android
  (use-package csproj-mode
    :ensure t
    :config
    (add-to-list 'auto-mode-alist '("\\.csproj\\'" . csproj-mode))
    (add-hook 'csproj-mode-hook #'aggressive-indent-mode)))

(when mymy-is-not-android
  (use-package csharp-ts-mode
    :no-require t
    :ensure nil
    :config
    (defun mymy-csharp-mode-hook ()
      (setq-local flycheck-navigation-minimum-level 'error))
    (add-hook 'csharp-ts-mode-hook #'mymy-csharp-mode-hook)
    ;; (add-hook 'csharp-ts-mode-hook #'subword-mode)

    (with-eval-after-load 'compile
      ;; Add support for going to line of error in stacktrace.
      (add-to-list 'compilation-error-regexp-alist 'dotnet-stack-trace)

      (add-to-list 'compilation-error-regexp-alist-alist
                   '(dotnet-stack-trace
                     "\\(?:^\\|\\s-+\\)at\\s-+\\(.*\\)\\s-+in\\s-+\\(.+\\):line\\s-+\\([0-9]+\\)"
                     2 3 nil 2 1)))))

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

   `diff-hl-dired-mode 'provides similar functionality in Dired.

   `diff-hl-margin-mode 'changes the highlighting function to use the
   margin instead of the fringe.

   `diff-hl-amend-mode 'sets the reference revision to the one before
   recent one. Also ,you could use diff-hl-set-reference-rev to set it to
   any revision ,see its docstring for details.

   `diff-hl-flydiff-mode 'implements highlighting changes on the fly.

   `diff-hl-show-hunk-mouse-mode 'makes fringe and margin react to mouse
   clicks to show the corresponding hunk. That 's the alternative to using
   diff-hl-show-hunk and friends.))

;; * Ripgrep
(use-package ag
  :ensure t)

;; * Multiple cursors
(use-package evil-mc
  :ensure (evil-mc :host github :repo "cheerio-pixel/evil-mc" :branch "feature/different-cursor-prefix")
  :init
  (setq evil-mc-cursors-keymap-prefix "gc")
  ;; Cool package but insane keybindings. Maybe, keyword: maybe, will pull request for optional keybindings.
  (defvar evil-mc-cursors-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "m") 'evil-mc-make-all-cursors)
      (define-key map (kbd "u") 'evil-mc-undo-last-added-cursor)
      (define-key map (kbd "q") 'evil-mc-undo-all-cursors)
      (define-key map (kbd "s") 'evil-mc-pause-cursors)
      (define-key map (kbd "r") 'evil-mc-resume-cursors)
      (define-key map (kbd "f") 'evil-mc-make-and-goto-first-cursor)
      (define-key map (kbd "l") 'evil-mc-make-and-goto-last-cursor)
      (define-key map (kbd "h") 'evil-mc-make-cursor-here)
      (define-key map (kbd "j") 'evil-mc-make-cursor-move-next-line)
      (define-key map (kbd "k") 'evil-mc-make-cursor-move-prev-line)
      (define-key map (kbd "N") 'evil-mc-skip-and-goto-next-cursor)
      (define-key map (kbd "P") 'evil-mc-skip-and-goto-prev-cursor)
      (define-key map (kbd "n") 'evil-mc-skip-and-goto-next-match)
      (define-key map (kbd "p") 'evil-mc-skip-and-goto-prev-match)
      (define-key map (kbd "I") 'evil-mc-make-cursor-in-visual-selection-beg)
      (define-key map (kbd "A") 'evil-mc-make-cursor-in-visual-selection-end)
      map))

  (el-patch-defvar evil-mc-key-map
    (let ((map (make-sparse-keymap)))
      (evil-define-key* '(normal visual) map
        (kbd evil-mc-cursors-keymap-prefix) evil-mc-cursors-map
        (el-patch-remove
          (kbd "M-n") 'evil-mc-make-and-goto-next-cursor
          (kbd "M-p") 'evil-mc-make-and-goto-prev-cursor
          (kbd "C-n") 'evil-mc-make-and-goto-next-match
          (kbd "C-t") 'evil-mc-skip-and-goto-next-match
          (kbd "C-p") 'evil-mc-make-and-goto-prev-match))
      map))
  :config
  (global-evil-mc-mode)
  (setq evil-mc-undo-cursors-on-keyboard-quit t)
  ;; (evil-define-key 'visual evil-mc-key-map
  ;;   "A" #'evil-mc-make-cursor-in-visual-selection-end
  ;;   "I" #'evil-mc-make-cursor-in-visual-selection-beg)
  )


;; * CSV mode
(use-package csv-mode
  :ensure t
  :hook (csv-mode . csv-align-mode))


;; * Common lisp
(when mymy-is-not-android
  (use-package sly
    :ensure t
    :config
    ;; (general-define-key
    ;;  :keymaps 'lisp-mode-map
    ;;  "M-<f3>" '
    ;;  )
    (add-to-list 'display-buffer-alist
                 '("\\*sly-\\(description\\|db\\)"
                   (display-buffer-reuse-window display-buffer-in-direction)
                   (direction . right)
                   (window-height . 0.43)
                   ))
    (add-to-list 'display-buffer-alist
                 '("\\*sly-mrepl"
                   (display-buffer-reuse-window display-buffer-in-direction)
                   (direction . bottom)
                   (dedicated . t)
                   (reusable-frames . visible)
                   (window-height . 0.37)))
    (setq inferior-lisp-program "/usr/bin/sbcl --dynamic-space-size 1024")
    (setq sly-lisp-implementations
          '((sbcl ("sbcl" "--dynamic-space-size" "1024"))))))


;; * Web mode
(when mymy-is-not-android
  (use-package web-mode
    :ensure t
    :init
    ;; Neat trick
    (define-derived-mode vue-web-mode web-mode "Vue")
    (define-derived-mode razor-web-mode web-mode "Razor")
    (define-derived-mode php-web-mode web-mode "PHP")
    (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-web-mode))

    ;; CSharp
    (add-to-list 'auto-mode-alist '("\\.razor\\'" . razor-web-mode))
    (add-to-list 'auto-mode-alist '("\\.cshtml\\'" . razor-web-mode))

    (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

    (add-to-list 'auto-mode-alist '("\\.php\\'" . php-web-mode))
    ;; (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
    ;; (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

    ;; (add-hook 'web-mode-hook #'emmet-mode)
    (add-hook 'vue-web-mode-hook #'mymy-vue-hook)
    (add-hook 'php-web-mode-hook #'lsp)))

(when mymy-is-not-android
  (use-package jtsx
    :ensure t
    :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
           ("\\.tsx\\'" . jtsx-tsx-mode)
           ("\\.ts\\'" . jtsx-typescript-mode))
    :commands jtsx-install-treesit-language
    :hook ((jtsx-jsx-mode . hs-minor-mode)
           (jtsx-tsx-mode . hs-minor-mode)
           (jtsx-typescript-mode . hs-minor-mode))
    :config
    ;; Optional customizations
    (gsetq js-indent-level 2)
    (gsetq typescript-ts-mode-indent-offset 4)
    (gsetq jtsx-switch-indent-offset 0)
    (gsetq jtsx-indent-statement-block-regarding-standalone-parent nil)
    (gsetq jtsx-jsx-element-move-allow-step-out t)
    (gsetq jtsx-enable-jsx-electric-closing-element t)
    (gsetq jtsx-enable-electric-open-newline-between-jsx-element-tags t)
    (gsetq jtsx-enable-jsx-element-tags-auto-sync nil)
    (gsetq jtsx-enable-all-syntax-highlighting-features t)
    (defun jtsx-bind-keys-to-mode-map (mode-map)
      "Bind keys to MODE-MAP."
      (define-key mode-map (kbd "C-c C-j") 'jtsx-jump-jsx-element-tag-dwim)
      (define-key mode-map (kbd "C-c j o") 'jtsx-jump-jsx-opening-tag)
      (define-key mode-map (kbd "C-c j c") 'jtsx-jump-jsx-closing-tag)
      (define-key mode-map (kbd "C-c j r") 'jtsx-rename-jsx-element)
      (define-key mode-map (kbd "C-c <down>") 'jtsx-move-jsx-element-tag-forward)
      (define-key mode-map (kbd "C-c <up>") 'jtsx-move-jsx-element-tag-backward)
      (define-key mode-map (kbd "C-c C-<down>") 'jtsx-move-jsx-element-forward)
      (define-key mode-map (kbd "C-c C-<up>") 'jtsx-move-jsx-element-backward)
      (define-key mode-map (kbd "C-c C-S-<down>") 'jtsx-move-jsx-element-step-in-forward)
      (define-key mode-map (kbd "C-c C-S-<up>") 'jtsx-move-jsx-element-step-in-backward)
      (define-key mode-map (kbd "C-c j w") 'jtsx-wrap-in-jsx-element)
      (define-key mode-map (kbd "C-c j u") 'jtsx-unwrap-jsx)
      (define-key mode-map (kbd "C-c j d") 'jtsx-delete-jsx-node)
      (define-key mode-map (kbd "C-c j t") 'jtsx-toggle-jsx-attributes-orientation)
      (define-key mode-map (kbd "C-c j h") 'jtsx-rearrange-jsx-attributes-horizontally)
      (define-key mode-map (kbd "C-c j v") 'jtsx-rearrange-jsx-attributes-vertically))
    (defun jtsx-bind-keys-to-jtsx-jsx-mode-map ()
      (jtsx-bind-keys-to-mode-map jtsx-jsx-mode-map))

    (defun jtsx-bind-keys-to-jtsx-tsx-mode-map ()
      (jtsx-bind-keys-to-mode-map jtsx-tsx-mode-map))

    (add-hook 'jtsx-jsx-mode-hook 'jtsx-bind-keys-to-jtsx-jsx-mode-map)
    (add-hook 'jtsx-tsx-mode-hook 'jtsx-bind-keys-to-jtsx-tsx-mode-map))
  )

;; * Ispell/Aspell
(when (and mymy-is-not-android mymy-we-are-not-at-work)
  (use-package ispell
    :ensure nil
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
      (write-region "" nil ispell-personal-dictionary nil 0))))

(when (and mymy-is-not-android mymy-we-are-not-at-work)
  (use-package jinx
    :ensure t
    :config
    (dolist (hook '(text-mode-hook))
      (add-hook hook #'jinx-mode))
    (general-define-key
     :states '(normal)
     :keymaps 'text-mode-map
     "z=" 'jinx-correct)
    :bind (("M-$" . jinx-correct)
           ("C-M-$" . jinx-languages))))


;; * Restclient
(when mymy-is-not-android
  (use-package restclient
    :ensure (restclient :files ("*.el"))
    ;; :ensure t
    :after (jq-mode)
    :config
    (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))
    (require 'restclient-jq)))

(when mymy-is-not-android
  (use-package jq-mode
    :ensure t))

;; * Nix
(when mymy-is-not-android
  (use-package nix-ts-mode
    :ensure t
    :mode "\\.nix\\'"))


;; * Harpoon
(when mymy-is-not-android
  (use-package harpoon
    ;; Nah, feel like the other approach I was going can adapt to emacs
    :disabled
    :ensure t
    :config
    (general-define-key
     :states '(normal motion visual)
     :prefix "SPC"
     :keymaps 'global-map
     "jj" 'harpoon-add-file
     "jc" 'harpoon-clear
     "jf" 'harpoon-toggle-file
     "1" 'harpoon-go-to-1
     "2" 'harpoon-go-to-2
     "3" 'harpoon-go-to-3
     "4" 'harpoon-go-to-4
     )

    (global-set-key (kbd "C-c h <return>") 'harpoon-add-file)
    (global-set-key (kbd "C-c h m") 'harpoon-toggle-quick-menu)
    (global-set-key (kbd "C-c h c") 'harpoon-clear)
    ;; (global-set-key (kbd "C-c h 1") 'harpoon-go-to-1)
    ;; (global-set-key (kbd "C-c h 2") 'harpoon-go-to-2)
    ;; (global-set-key (kbd "C-c h 3") 'harpoon-go-to-3)
    ;; (global-set-key (kbd "C-c h 4") 'harpoon-go-to-4)

    ;; (global-set-key (kbd "C-c h f") 'harpoon-toggle-file)
    ;; (global-set-key (kbd "C-c h h") 'harpoon-toggle-quick-menu)
    ;; (global-set-key (kbd "C-c h c") 'harpoon-clear)
    ;; (global-set-key (kbd "C-c h 1") 'harpoon-go-to-1)
    ;; (global-set-key (kbd "C-c h 2") 'harpoon-go-to-2)
    ;; (global-set-key (kbd "C-c h 3") 'harpoon-go-to-3)
    ;; (global-set-key (kbd "C-c h 4") 'harpoon-go-to-4)
    ))

(use-package bookmark-harpoon
  ;; Making this work with bookmark is kind of difficult. If we can
  ;; intgrate with jump operation then we can create any type of record and
  ;; jump to it.
  :disabled
  :after (bookmark+)
  :ensure nil
  :no-require t
  :config

  (defcustom mymy-bookmark-harpoon-project-function #'projectile-project-root
    "Function that returns the path of the root of the current project.")

  (defcustom mymy-bookmark-harpoon-get-branch-name #'mymy-bookmark-harpoon--get-git-branch-name
    "Function that returns the current branch of the project. If function
     is nil then branches are not taken into account.")

  (defcustom mymy-bookmark-harpoon-file (concat
                                         user-emacs-directory
                                         "bookmarks_harpoons")
    "File to save bookmarks.")

  (defcustom mymy-bookmark-harpoon-bookmark-file-function #'bmkp-make-record-for-target-file
    "Function that takes a file and returns a function that gets called
    when we make a new bookmark record.")

  (defvar mymy-bookmark-harpoon-is-loaded nil
    "Set when the bookmark file is loaded.")

  (defconst mymy-bookmark-harpoon-bookmark-alist nil
    "Current list of bookmarks.")

  (comment
   bmkp-set-sequence-bookmark
   )

  ;; Taken from harpoon.el
  (defun mymy-bookmark-harpoon--get-git-branch-name ()
    "Get the branch name for harpoon."
    (car (split-string
          (shell-command-to-string
           (concat "cd " (funcall mymy-bookmark-harpoon-project-function) "; git rev-parse --abbrev-ref HEAD"))
          "\n")))

  (defun mymy-bookmark-harpoon--make-tag (&optional file)
    "Returns a the root project directory with the branch name."
    (let ((default-directory (thread-last
                               (or file default-directory)
                               (file-name-directory)
                               (directory-file-name))))
      (s-join
       "@"
       (list
        (funcall mymy-bookmark-harpoon-project-function)
        (funcall mymy-bookmark-harpoon-get-branch-name)))))

  (defsubst mymy-bookmark-harpoon--put (project bookmarks)
    "Associate a project with a list of bookmark records."
    (puthash project bookmarks mymy-bookmark-harpoon-bookmark-alist)
    )

  (defsubst mymy-bookmark-harpoon--get (project)
    "Gets the list of bookmark records from project."
    (gethash project mymy-bookmark-harpoon-bookmark-alist)
    )

  (defsubst mymy-bookmark-harpoon--new ()
    "Return new object of key-value pairs."
    (make-hash-table
     :test 'equal))

  (gv-define-simple-setter
   mymy-bookmark-harpoon--get
   mymy-bookmark-harpoon--put
   )

  (defun mymy-bookmark-harpoon--load ()
    "Unserialize `mymy-bookmark-harpoon-file' contents."
    (let ((file mymy-bookmark-harpoon-file))
      (setq mymy-bookmark-harpoon-bookmark-alist
            (if (file-exists-p file)
                (read (find-file-noselect file))
              (let ((map (mymy-bookmark-harpoon--new)))
                (mymy-bookmark-harpoon--save map)
                map)))))

  (defun mymy-bookmark-harpoon--save (&optional data)
    "Serialize DATA or `mymy-bookmark-harpoon-bookmark-alist' contents
`mymy-bookmark-harpoon-file'."
    (let ((file mymy-bookmark-harpoon-file)
          (data (or data mymy-bookmark-harpoon-bookmark-alist)))
      (if (file-writable-p file)
          (with-temp-file file
            (insert (let (print-length) (prin1-to-string data))))
        (message "File '%s' not writeable" filename))))

  (defun mymy-bookmark-harpoon--maybe-load ()
    "Load mymy-bookmark-harpoon-bookmark-alist if is not already loaded"
    (let (data)
      (and (null mymy-bookmark-harpoon-is-loaded)
           (null mymy-bookmark-harpoon-bookmark-alist)
           (setq data (mymy-bookmark-harpoon--load))
           (setq mymy-bookmark-harpoon-bookmark-alist data)
           (setq mymy-bookmark-harpoon-is-loaded t))))

  (defun mymy-bookmark-harpoon-jump (bookmark)
    ;; (cl-letf (((symbol-function 'bmkp-goto-position)
    ;;            (lambda (_ file &rest ignore)
    ;;              (find-file file)))
    ;;           ((symbol-function 'bmkp-get-bookmark)
    ;;            (lambda (&rest ignore) bookmark)
    ;;            )
    ;;           )
    ;;   (bookmark--jump-via bookmark 'bmkp--pop-to-buffer-same-window))
    (cond
     ((bookmark-get-filename bookmark)
      (find-file (bookmark-get-filename bookmark))
      )
     )
    )

  (defun mymy-bookmark-harpoon-append-file (file)
    (interactive
     (if-let (curr-file (buffer-file-name))
         (list curr-file)
       (user-error "Current buffer is not associated with any file.")))
    (mymy-bookmark-harpoon--maybe-load)
    (with-current-buffer (find-file-noselect file)
      (let ((bookmark-make-record-function (funcall mymy-bookmark-harpoon-bookmark-file-function file)))
        (cl-pushnew
         (list
          (file-name-nondirectory (directory-file-name file))
          (cdr (bookmark-make-record)))
         (thread-last file
                      (mymy-bookmark-harpoon--make-tag)
                      (mymy-bookmark-harpoon--get))))))

  (defun mymy-consult-select-by-number (candidates)
    (let ((numbered-candidates
           (cl-loop for candidate in candidates
                    for i from 0
                    collect (cons (format "%d %s" i (car candidate)) (cdr candidate)))))
      (lambda (string predicate action)
        (if (string-match "^[0-9]+$" string)
            (let ((index (string-to-number string)))
              (if (< index (length numbered-candidates))
                  (if (eq action 'metadata)
                      `(metadata (category . consult-number-selection))
                    (cons 'return (nth index candidates)))
                nil))
          (funcall (consult--completion-table numbered-candidates) string predicate action)))))

  (defun mymy-consult-bookmark-harpoon ()
    (mymy-bookmark-harpoon--maybe-load)
    (thread-last
      (consult--read
       (thread-last (mymy-bookmark-harpoon--make-tag)
                    (mymy-bookmark-harpoon--get))
       :prompt "Go to: "
       :require-match t
       :lookup (lambda (cand candidates &rest ignore)
                 (mymy-bookmark-harpoon-jump
                  (alist-get cand candidates)))
       )
      )
    )

  (comment
   (projectile-project-root)
   bmkp-switch-bookmark-file-create)
  ;; Save file path, tag with project root path, git branch and number
  ;; If branch saving is disabled,

  )

(when mymy-is-not-android
  (use-package bookmark+
    :ensure nil
    :no-require t
    :config
    (let ((bookmarkplus-dir (concat user-emacs-directory "custom/bookmark-plus/"))
          (emacswiki-base "https://www.emacswiki.org/emacs/download/")
          (bookmark-files '("bookmark+.el" "bookmark+-mac.el" "bookmark+-bmu.el" "bookmark+-key.el" "bookmark+-lit.el" "bookmark+-1.el")))
      (require 'url)
      (add-to-list 'load-path bookmarkplus-dir)
      (make-directory bookmarkplus-dir t)
      (mapcar (lambda (arg)
                (let ((local-file (concat bookmarkplus-dir arg)))
                  (unless (file-exists-p local-file)
                    (url-copy-file (concat emacswiki-base arg) local-file t))))
              bookmark-files)
      (require 'bookmark+-mac)
      (byte-recompile-directory bookmarkplus-dir 0)
      (require 'bookmark+))
    (defvar mymy-quick-access-tag "quick_access")

    (defun mymy-quick-access ()
      (interactive)
      (let ((alist (bmkp-some-tags-alist-only (list mymy-quick-access-tag))))
        (thread-last
          (bookmark-completing-read "Bookmark" (bmkp-default-bookmark-name alist) alist)
          ;; (bookmark-jump)
          (bookmark-get-filename)
          (find-file))))
    (general-define-key
     :prefix "C-c h"
     "b" #'consult-bookmark
     "f" #'mymy-quick-access)))

(use-package dogears
  :disabled
  :ensure (dogears :fetcher github :repo "alphapapa/dogears.el"
                   :files (:defaults (:exclude "helm-dogears.el")))

  ;; These bindings are optional, of course:
  :bind (:map global-map
              ("M-g d" . dogears-go)
              ("M-g M-b" . dogears-back)
              ("M-g M-f" . dogears-forward)
              ("M-g M-d" . dogears-list)
              ("M-g M-D" . dogears-sidebar))
  :config
  (dogears-mode))

;; * Lua
(when mymy-is-not-android
  (use-package lua-mode
    :ensure t
    :mode "\\.lua$"
    :hook (lua-mode . lsp)
    :config
    (with-eval-after-load 'lsp-mode
      ;; Loading some neovim libraries to pry vim plugins
      (setq lsp-lua-workspace-library
            (ht ("/usr/share/nvim/runtime/lua" t))))))

;; * Leetcode
(when mymy-is-not-android
  (use-package leetcode
    :ensure t
    :init
    ;; (setq leetcode-prefer-language "python3")
    (setq leetcode-prefer-language "c")
    (setq leetcode-prefer-sql "mysql")
    (setq leetcode-save-solutions t)
    (setq leetcode-directory (concat main-dropbox-dir
                                     "leecode"
                                     ))
    ))

;; * Zig
(when mymy-is-not-android
  (use-package zig-mode
    :ensure t
    :config
    ;; Slow on river project, don't know why
    (setq zig-format-on-save nil)))

(when mymy-is-not-android
  (use-package lsp-zig
    :after (zig-mode lsp-mode)
    :ensure nil
    :no-require t
    :hook (zig-mode . lsp)))

;; * tab-bar
(when mymy-is-not-android
  (use-package tab-bar
    ;; From https://www.reddit.com/r/emacs/comments/r16adq/comment/hlxl1ek/
    :bind (:map tab-prefix-map ("p" . my/new-project-tab))
    :init
    (defun my/new-project-tab ()
      (interactive)
      (other-tab-prefix)
      (projectile-switch-project)
      (tab-rename (projectile-project-name)))
    :config
    (gsetq tab-bar-show nil)
    (tab-bar-mode)
    ))

;; * Ace window
(when mymy-is-not-android
  (use-package ace-window
    :ensure t
    :init
    ;;global could be cool if I didn't use a tiling window manager.
    (setq aw-scope 'frame)
    ;; (setq aw-keys '(?a ?r ?s ?t ?h ?n ?e ?i ?o))
    (setq aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
    (setq aw-ignore-current nil)
    :config
    (general-define-key
     :keymaps 'override
     ;; "M-o" 'ace-window
     ;; "C-x o" 'ace-window
     "M-u" 'ace-window
     ;; "M-y" 'ace-window
     )))


;; * Vterm
(when mymy-is-not-android
  (use-package vterm
    :disabled
    :ensure t
    :demand t
    :config
    (defun mymy-terminal-configuration-hook ()
      (setq-local show-trailing-whitespace nil)
      (setq-local display-line-numbers nil)
      (setq-local mode-line-format
                  '((:eval
                     (mymy-mode-line-word-with-padding
                      (concat
                       (when (equal (buffer-name)
                                    (multi-vterm-dedicated-get-buffer-name))
                         "Dedicated ")
                       "VTerm")
                      )
                     )))
      ;; Let's not mix evil and emacs
      (evil-emacs-state)
      )

    (defun vterm-directory-sync ()
      "Synchronize current working directory."
      (interactive)
      (when vterm--process
        (let* ((pid (process-id vterm--process))
               (dir (file-truename (format "/proc/%d/cwd/" pid))))
          (setq default-directory dir))))

    (defun mymy-vterm-copy-hook ()
      (if vterm-copy-mode
          (evil-motion-state)
        (evil-emacs-state)))

    (defun vterm-directory-sync ()
      "Synchronize current working directory."
      (interactive)
      (when vterm--process
        (let* ((pid (process-id vterm--process))
               (dir (file-truename (format "/proc/%d/cwd/" pid))))
          (setq default-directory dir))))

    (defun mymy-vterm-find-file ()
      "Simple wrapper around"
      (interactive)
      (vterm-directory-sync)
      (call-interactively #'find-file)
      )

    (defun mymy-vterm-change-directory-to-last-buffer ()
      "Change vterm's directory to the directory of the last visited buffer."
      (interactive)
      (let* ((last-buffer (other-buffer (current-buffer) t))
             (last-buffer-dir (with-current-buffer last-buffer
                                (expand-file-name default-directory))))
        (when (and (buffer-live-p last-buffer)
                   (not (eq (buffer-local-value 'major-mode last-buffer) 'vterm-mode)))
          ;; Space before cd so that in zsh itsn't saved in history
          (vterm-send-string (concat " cd " (shell-quote-argument last-buffer-dir)))
          (vterm-send-return))))

    ;; Show vterm terminals at the bottom, courtesy of
    ;; https://github.com/jixiuf/vterm-toggle/issues/33#issuecomment-1098390727
    (add-to-list 'display-buffer-alist
                 '("\\*vterm"
                   (display-buffer-reuse-window display-buffer-in-direction)
                   (direction . top)
                   (dedicated . t)
                   (mode . vterm-mode)
                   (reusable-frames . visible)
                   (window-height . 0.3)))

    (general-define-key
     :keymaps 'global
     "M-<f1>" #'multi-vterm-project
     "M-<f2>" #'mymy-multi-vterm-dedicated-toggle)

    (general-define-key
     :keymaps 'vterm-mode-map
     ;; Enter and never allow to go back
     "C-z" #'vterm--self-insert
     "C-q" #'vterm-send-next-key
     ;; For some reason shell side configuration doesn't work
     "C-x C-f" #'mymy-vterm-find-file
     "C-c C-d" #'mymy-vterm-change-directory-to-last-buffer
     )

    (general-define-key
     :states '(motion emacs)
     :keymaps 'vterm-mode-map
     ;; Enter and never allow to go back
     "C-z" #'vterm--self-insert
     )

    :hook
    (vterm-mode . mymy-terminal-configuration-hook)
    (vterm-copy-mode . mymy-vterm-copy-hook)))

(when mymy-is-not-android
  (use-package multi-vterm
    :disabled
    :after (vterm)
    :ensure t
    :init
    (defun mymy-multi-vterm-dedicated-toggle ()
      "Toggle dedicated vterm buffer while respecting displa-buffer-alist"
      (interactive)
      (let ((dedicated-buffer (multi-vterm-get-buffer 'dedicated)))
        (if-let (dedicated-window (get-buffer-window dedicated-buffer))
            (if (eq dedicated-window (selected-window))
                ;; Shown and focused, delete it
                (delete-window dedicated-window)
              ;; Not focused, then focus
              (select-window dedicated-window))
          ;; Now shown, then show it
          (select-window (display-buffer dedicated-buffer)))))
    ))

;; * Python
(when mymy-is-not-android
  (use-package poetry
    :ensure t
;;; TODO: Need to solve the initial lag when opening a pyhton file.
    :config
    ;; This is necessary for anything that uses virtual envs
    (setenv "WORKON_HOME" "~/.cache/pypoetry/virtualenvs/")
    (add-to-list 'display-buffer-alist
                 '("\\*poetry\\*"
                   (display-buffer-no-window)
                   ))
    ;; (general-define-key
    ;;  "C-c c" 'poetry)
    ;; :hook
    ;; (python-mode . poetry-tracking-mode)
    ))

(when mymy-is-not-android
  (use-package pdf-tools
    :ensure t
    :config
    (pdf-loader-install) ; On demand loading, leads to faster startup time
    ))

(use-package saveplace-pdf-view
  :ensure t
  :after (pdf-tools)
  :demand t
  :config
  ;; Not loading for some reason
  (require 'bookmark)
  (require 'saveplace-pdf-view)
  (save-place-mode 1)
  )

(when mymy-is-not-android
  (use-package org-noter
    :after (pdf-tools)
    :ensure t
    :config
    (setq org-noter-doc-split-percentage '(0.7 . 0.3))))


;; * Scala
(when mymy-is-not-android
  (use-package scala-mode
    :disabled t
    :ensure t
    :interpreter
    ("scala" . scala-mode)
    :config
    (defun mymy-scala-hook ()
      (add-to-list
       'compilation-error-regexp-alist-alist
       '(scala-stacktrace
         "^\\[error\\] \\([.a-zA-Z0-9_/\\\\-]+[.scala]\\):\\([0-9]+\\):\\([0-9]+\\):"
         1 2 3 2 1)
       )
      (add-to-list
       'compilation-error-regexp-alist
       'scala-stacktrace
       )
      )
    :hook
    (scala-mode . mymy-scala-hook)
    (scala-mode . lsp)
    (sbt-mode . mymy-scala-hook)
    )
  (use-package scala-ts-mode
    :ensure t
    :config
    (defun mymy-scala-ts-hook ()
      (setq-local treesit-font-lock-level 4)
      (treesit-font-lock-recompute-features)
      (setq-local lsp-semantic-tokens-apply-modifiers nil)
      ;; (setq-local lsp-semantic-tokens-enable t)
      )

    (add-hook 'scala-ts-mode-hook #'mymy-scala-ts-hook)
    :hook
    (scala-ts-mode . lsp)
    )

  (use-package polymode
    ;; Doesn't work, for some reason. Tested in minimal setup, works there
    :disabled t
    :ensure t
    :demand t
    :config
    (define-hostmode poly-scala-ts-hostmode nil
                     ""
                     :mode 'scala-ts-mode
                     )

    (define-innermode poly-sql-expr-scala-innermode nil
                      ""
                      :mode 'sql-mode
                      :head-matcher (rx "sql"
                                        (= 3 (char "\"'"))
                                        (* (any space))
                                        )
                      :tail-matcher (rx
                                     (= 3 (char "\"'")
                                        )
                                     )
                      :head-mode 'host
                      :tail-mode 'host
                      )

    (define-polymode poly-scala-ts-sql-mode nil
                     ""
                     :hostmode 'poly-scala-ts-hostmode
                     :innermodes '(poly-sql-expr-scala-innermode)
                     )
    )

  (with-eval-after-load 'projectile
    (projectile-register-project-type 'mymysbt '("build.sbt")
                                      :project-file "build.sbt"
                                      :src-dir "main"
                                      :test-dir "test"
                                      :run "sbtn run"
                                      :compile "sbtn compile"
                                      :test "sbtn test"
                                      :test-suffix "Suite")
    )

  (use-package sbt-mode
    :ensure t
    :commands sbt-start sbt-command
    ;; :config
    ;; WORKAROUND: allows using SPACE when in the minibuffer
    ;; (substitute-key-definition
    ;;  'minibuffer-complete-word
    ;;  'self-insert-command
    ;;  minibuffer-local-completion-map)

    ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
    ;; (setq sbt:program-options '("-Dsbt.supershell=false"))
    )
  ;; Add metals backend for lsp-mode
  (use-package lsp-metals
    :after (lsp-mode)
    :ensure t
    :init
    (setq lsp-metals-server-command
          (expand-file-name
           "~/.local/share/coursier/bin/metals"
           ))

    ;; You might set metals server options via -J arguments. This might not always work, for instance when
    ;; metals is installed using nix. In this case you can use JAVA_TOOL_OPTIONS environment variable.
    (setq lsp-metals-server-args '(;; Metals claims to support range formatting by default but it supports range
                                   ;; formatting of multiline strings only. You might want to disable it so that
                                   ;; emacs can use indentation provided by scala-mode.
                                   "-J-Dmetals.allow-multiline-string-formatting=off"
                                   ;; Enable unicode icons. But be warned that emacs might not render unicode
                                   ;; correctly in all cases.
                                   "-J-Dmetals.icons=unicode"
                                   "-J-Dmetals.client=emacs"
                                   "-J-XX:+UseG1GC"
                                   "-J-XX:+UseStringDeduplication"
                                   ;; "-J-Xss4m"
                                   ;; "-J-Xms100m"
                                   ))
    ;; In case you want semantic highlighting. This also has to be enabled in lsp-mode using
    ;; `lsp-semantic-tokens-enable' variable. Also you might want to disable highlighting of modifiers
    ;; setting `lsp-semantic-tokens-apply-modifiers' to `nil' because metals sends `abstract' modifier
    ;; which is mapped to `keyword' face.
    (setq lsp-metals-enable-semantic-highlighting t)
    ))

;; * Dart

(use-package dart-mode
  :defer 5
  :ensure t
  :hook (dart-mode . lsp)
  )

(use-package lsp-dart
  :ensure t
  :after (dart-mode lsp)
  :config
  (defun lsp-dart-dap--populate-flutter-start-file-args (conf)
    "Populate CONF with the required arguments for Flutter debug."
    (let ((pre-conf (-> conf
                        lsp-dart-dap--base-debugger-args
                        (dap--put-if-absent :type "flutter")
                        (dap--put-if-absent :flutterMode "debug")
                        (dap--put-if-absent :program (or (lsp-dart-get-project-entrypoint)
                                                         (buffer-file-name))))))
      (lambda (start-debugging-callback)
        (lsp-dart-dap--flutter-get-or-start-device
         (-lambda (args)
           (let ((device-id (lsp-get args :id))
                 (device-name (lsp-get args :name)))
             (funcall start-debugging-callback
                      (-> pre-conf
                          (dap--put-if-absent :deviceId device-id)
                          (dap--put-if-absent :deviceName device-name)
                          (dap--put-if-absent :dap-server-path (if (lsp-dart-dap-use-sdk-debugger-p)
                                                                   (append (lsp-dart-flutter-command) (list "debug_adapter" "-d" device-id))
                                                                 lsp-dart-dap-flutter-debugger-program))
                          (dap--put-if-absent :flutterPlatform "default")
                          (dap--put-if-absent :toolArgs `("-d" ,device-id))
                          (dap--put-if-absent :name (concat "Flutter (" device-name ")"))))
             ))))))
  )


;; * Yaml
(use-package yaml-mode
  :ensure t
  )
