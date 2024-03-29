﻿;; -*- lexical-binding: t; -*-
;;* Early
;; https://github.com/bkaestner/.emacs.d/blob/37c75bfe3a199594ad89504d870e68f6f424764f/early-init.el
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)
;; After Emacs has completely started, reset the values to more sensible ones.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 100000000
                  gc-cons-percentage 0.1)))

(setq straight-check-for-modifications '(check-on-save find-when-checking))

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
(straight-use-package 'org)
(straight-use-package 'use-package)
(eval-when-compile (require 'use-package))
;;* Require
(require 'cl-lib)

;;* Load path
(add-to-list 'load-path "~/.emacs.d/elisp/")

;;* Important packages
;; This packages need to be loaded before all the others

(use-package general)

;; Explicit patching of functions and variables.
(use-package el-patch)

;; Bring a little bit of clojure and more
(use-package dash :config (global-dash-fontify-mode))

;;* Alias
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'gsetq 'general-setq)
(defalias 'gsetq-local 'general-setq-local)
(defalias 'gsetq-default 'general-setq-default)

;;* Configs
(set-frame-parameter nil 'fullscreen 'fullboth) ; Fullscreen
(setq redisplay-dont-pause t)
(setq frame-resize-pixelwise t)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(tool-bar-mode -1)   ; This is much easier
(menu-bar-mode -1)   ; than needing to change
(scroll-bar-mode -1) ; this on every OS
(setq byte-compile-warnings '(not obsolete));; Cl warnings
(setq custom-file "~/.emacs.d/custom-file.el")
(load custom-file)
(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)
(setq native-comp-async-report-warnings-errors 'silent)
(setq create-lockfiles nil)
(setq ring-bell-function 'ignore)
(setq abbrev-suggest t)

;;* Modes
(global-hl-line-mode)
(setq-default show-trailing-whitespace t)
;; Stopped being useful
;; (global-whitespace-mode)
(xterm-mouse-mode)
(savehist-mode)
(save-place-mode)
(setq-default abbrev-mode t)

;;* GC and minibuffer
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 100000000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)
;;* My variables
;; TODO: Set all of this in the defconst or defvar form
(setq dropbox-dir
       (pcase system-type
         ('windows-nt "c:/Users/frail/Dropbox/")
         ('gnu/linux "~/Dropbox (Maestral)/")))
(setq main-dropbox-dir (concat dropbox-dir "Creativè/"))
(setq mymy-org-roam-dir (concat main-dropbox-dir "Notes/"))

(defvar mymy-index-id "cd6174d3-3589-4286-8a1d-9f7254e22c33"
  "The org ID of my index note in `mymy-org-roam-dir'")

(with-eval-after-load 'org-roam
  (defvar mymy-index-node (org-roam-node-from-id mymy-index-id)
    "The org-roam node of my index note"))

(defvar mymy-organization-system-directory (concat main-dropbox-dir "info-files/")
  "General purpose root directory of notes")
;; Check
(when (file-exists-p mymy-organization-system-directory)
  (error "Cannot find '%s'. Directory doesn't exist " mymy-organization-system-directory))

;;* Variables

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;;* Abbrevs
(setq abbrev-file-name (concat dropbox-dir "emacs/abbrev_defs"))

;; (let ((mymy-abbrevs
;;        '(("bc" "because")
;;          ("wo" "without")
;;          ("ex" "For example,")
;;          ("zk" "Zettelkasten")
;;          ;; ("col" "collection")
;;          ;; ("perm" "permanent")
;;          ;; ("lit" "literature")
;;          ;; ("sd" "software development")
;;          ;; ("diff" "different")
;;          ("pv" "previous")
;;          ("bf" "before")
;;          ("hw" "however")
;;          )))
;;   (mapc (lambda (x) (define-global-abbrev (car x) (cadr x))) mymy-abbrevs))

;;* Macros

(defmacro comment (&rest args)
  "Clojure-style comment block"
  nil)

;;* Functions

(defun increment-number-at-point (arg)
  (interactive "p")
  (let ((old-point (point)))
    (unwind-protect
        (progn
          (skip-chars-backward "0-9")
          (or (looking-at "[0-9]+")
              (error "No number at point"))
          (replace-match (number-to-string (+ (or arg 1) (string-to-number (match-string 0))))))
      (goto-char old-point))))

(defun decrement-number-at-point (arg)
  (interactive "p")
  (let ((old-point (point)))
    (unwind-protect
        (progn
          (skip-chars-backward "0-9")
          (or (looking-at "[0-9]+")
              (error "No number at point"))
          (replace-match (number-to-string (- (string-to-number (match-string 0)) (or arg 1)))))
      (goto-char old-point))))


;; I use this too much
(defun mymy-kill-new (s)
  (kill-new (format "%S" s)))

(defvar killed-file-list nil
  "List of recently killed files.")

(defun add-file-to-killed-file-list ()
  "If buffer is associated with a file name, add that file to the
  `killed-file-list' when killing the buffer."
  (when buffer-file-name
    (push buffer-file-name killed-file-list)))

(add-hook 'kill-buffer-hook #'add-file-to-killed-file-list)

(defun reopen-killed-file ()
  "Reopen the most recently killed file, if one exists."
  (interactive)
  (if killed-file-list
      (find-file (pop killed-file-list))
    (error "No recently-killed files to reopen")))

(defun reopen-killed-file-fancy ()
  "Pick a file to revisit from a list of files killed during this
  Emacs session."
  (interactive)
  (if killed-file-list
      (let ((file (completing-read "Reopen killed file: " killed-file-list
                                   nil nil nil nil (car killed-file-list))))
        (when file
          (setq killed-file-list (cl-delete file killed-file-list :test #'equal))
          (find-file file)))
    (error "No recently-killed files to reopen")))

(defun my-save-word ()
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
  Move point to the first non-whitespace character on this line.
  If point is already there, move to the beginning of the line.
  Effectively toggle between the first non-whitespace character and
  the beginning of the line.
  If ARG is not nil or 1, move forward ARG - 1 lines first.  If
  point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;;** Delete functions
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
  With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))
(defun delete-word (arg)
  "Delete characters forwards until encountering the beginning of a word.
  With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-line (arg)
  "Delete (not kill) the current line, backwards from cursor.
  With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (beginning-of-visual-line arg) (point))))

(defun delete-line (arg)
  "Delete (not kill) the current line, forwards from cursor.
  With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (end-of-visual-line arg) (point))))

(defun my-delete-whole-line (&optional arg)
  "Delete current line.
  With prefix ARG, delete that many lines starting from the current line.
  If ARG is negative, delete backward.  Also delete the preceding newline.
  \(This is meant to make \\[repeat] work well with negative arguments.)
  If ARG is zero, delete current line but exclude the trailing newline."
  (interactive "p")
  (or arg (setq arg 1))
  (if (and (> arg 0) (eobp) (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0) (bobp) (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (cond ((zerop arg)
         ;; I just need to understand elisp to discard what it's not needed
         (save-excursion
           (delete-region (point) (progn (forward-visible-line 0) (point))))
         (delete-region (point) (progn (end-of-visible-line) (point))))
        ((< arg 0)
         (save-excursion
           (delete-region (point) (progn (end-of-visible-line) (point))))
         (delete-region (point)
                        (progn (forward-visible-line (1+ arg))
                               (unless (bobp) (backward-char))
                               (point))))
        (t
         (save-excursion
           (delete-region (point) (progn (forward-visible-line 0) (point))))
         (delete-region (point)
                        (progn (forward-visible-line arg) (point))))))

(defun duplicate-current-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline) ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count))))

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list))))) ; end-of-let
;; put the point in the lowest line and return
;; (next-line arg)

(defun replace-next-line ()
  "Replace the next line with kill ring."
  (interactive)
  (save-excursion
    (forward-line)
    (beginning-of-line)
    (delete-line 1)
    (yank)))

(defun replace-current-line ()
  "Replace current line with kill ring"
  (interactive)
  (beginning-of-line)
  (delete-line 1)
  (yank))

(defun paste-primary-selection ()
  (interactive)
  (insert (gui-get-primary-selection)))

(defun yank-at-point ()
  "Yank but do not move the point"
  (interactive)
  (yank)
  (pop-global-mark))

(defun smart-open-line ()
  "Insert an empty line after the current line.
  Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

(defun create-scratch-buffer nil
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

;;* Ryo modal

(use-package ryo-modal
  ;; DONE: Make ryo-modal-key create a function when passed a lambda
  ;; The name of the function will be from the keyword :lambda-name
  ;; So ryo-modal-key will check if TARGET is a lambda and then theck
  ;; if lambda-name exists, if not then complain
  :commands ryo-modal-mode
  :bind
  ("S-SPC" . ryo-modal-global-mode)
  ("ç" . ryo-modal-global-mode)
  ("C-t" . ryo-modal-global-mode)
  :config
  (setq ryo-modal-cursor-color nil)
  (setq ryo-modal-cursor-type 'hollow)
  :config
  (require 'ryo-modal-patch)

  (defun mymy-replace-ryo-modal-key-to-my-careful ()
    (unless (eq (symbol-function #'ryo-modal-key) 'mymy-ryo-modal-key-careful)
      (defalias 'old-ryo-modal-key (symbol-function #'ryo-modal-key)))
    (defun mymy-ryo-modal-key-careful (key target &rest args)
      "Use ryo-modal-key but first check if the key is already binded.
  If you are sure you want to override a binding pass :force as an arg."
      (when (memq :force args)
        (apply #'old-ryo-modal-key key target (remove :force args)))
      (let ((cmd (lookup-key (or (plist-get args :mode) ryo-modal-mode-map) (kbd key)))
            hash hash-name)

        (when (plist-get args :name)
          (setq hash (secure-hash 'md5 (format "%s%s" target args)))
          (setq hash-name (concat "ryo:" hash ":" (plist-get args :name))))
        (if (and cmd
                 (or (not (eq cmd target))
                     (not (eq hash-name (format "%s" cmd))))
                 (not (keymapp cmd)))
            (user-error (format "Do you want to rebind this shortcut? Old command: %s New command: %s" cmd target))
          (apply #'old-ryo-modal-key key target (remove :force args)))))
    (defalias 'ryo-modal-key #'mymy-ryo-modal-key-careful))

  ;; (mymy-replace-ryo-modal-key-to-my-careful)

  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Advice-Combinators.html
  ;; (defun ryo-modal-key-careful (orig-f &rest args)
  ;;   "Use ryo-modal-key but first check if the key is already binded.
  ;; If you are sure you want to override a binding pass :force as an arg."
  ;;   (let* ((key (car args))
  ;;          (target (cadr args))
  ;;          (cmd (lookup-key (or (plist-get args :mode) ryo-modal-mode-map) (kbd key))))
  ;;     (when (memq :force args)
  ;;       (apply orig-f key target (remove :force args)))
  ;;     (if (and cmd (not (or (string-match-p (format "%S" target) (format "%S" cmd))
  ;;                           (string-match-p (format "%s" (plist-get args :name)) (format "%S" cmd)))))
  ;;         (user-error "Do you want to rebind this shortcut?")
  ;;       (apply orig-f key target (remove :force args)))))

  ;; (advice-add #'ryo-modal-key :around #'ryo-modal-key-careful)
  ;; (ryo-modal-key "f" #'next-line)
  ;; (ryo-modal-key "f" #'iedit-mode)
  ;; Bug: Force doesn't work
  (defvar ryo-modal-excluded-modes '()
    "A list of modes that the global mode should exclude.
By default the minibuffer is excluded."
    )

  (define-globalized-minor-mode ryo-modal-global-mode ryo-modal-mode
    (lambda ()
      (when (and (not (minibufferp (current-buffer)))
                 (not (member major-mode ryo-modal-excluded-modes)))
        (ryo-modal-mode t))))
  :config
  ;; r, s, R, S are reserved for major modes
  (with-eval-after-load 'iedit-mode
    (ryo-modal-key "f" #'iedit-mode))
  (ryo-modal-keys
    ("C-\\" toggle-input-method :exit 1)
    ("gd" remember)
    ("gD" remember-notes)
    ("gi" quoted-insert)
    ("/" undo)
    ("p" keyboard-quit)
    ("," ryo-modal-repeat)
    ("m" newline)
    ("M" (("o" split-line)
          ("m" smart-open-line)
          ("n" open-line)))
    ("z" (("u" upcase-char)
          ("U" upcase-region)
          ("v" upcase-initials-region)
          ("n" downcase-dwim)
          ("+" increment-number-at-point)
          ("-" decrement-number-at-point)))
    ("Z" (("v" fill-region)
          ("s" my-save-word))))
  (ryo-modal-keys
    ("ot" (("s" bookmark-set)
           ("d" bookmark-delete))))
  (ryo-modal-keys ;;; Navigation
    (:norepeat t)
    ("n" next-line)
    ("u" previous-line)
    ("N" backward-char)
    ("U" forward-char)
    ("M-n" forward-sentence)
    ("M-u" backward-sentence)
    ("a" smarter-move-beginning-of-line)
    ("e" end-of-line)
    ("A" point-to-register)
    ;; ("E" jump-to-register)
    ("E" (lambda () (interactive)
           (cond ((looking-at "\\s\(")
                  (forward-list))
                 ((looking-back "\\s\)")
                  (backward-list))
                 (t nil)))
     :name "lispy-different")
    ("c" forward-to-word)
    ("C" forward-word)
    ("X" backward-to-word)
    ("x" backward-word)
    ("g" (("a" beginning-of-buffer)
          ("e" end-of-buffer)
          ("u" scroll-down)
          ("n" scroll-up))))
  ;; delete
  (ryo-modal-keys
    ("D" (("t" my-delete-whole-line)
          ("d" backward-delete-word)
          ("v" delete-region)
          ("f" delete-word)
          ("s" delete-char)
          ("r" backward-delete-char)
          ("h" delete-line)
          ("(" delete-pair)
          ("SPC" delete-horizontal-space)
          ("/" delete-blank-lines))))
  ;; kill
  (ryo-modal-keys
    ("k" kill-line)
    ("K" (("a" kill-whole-line)
          ("v" kill-region)
          ("w" kill-ring-save))))
  ;; yank
  (ryo-modal-keys
    ("y" yank)
    ("Y" (("an" duplicate-current-line)
          ("nd" replace-next-line)
          ("ad" replace-current-line)
          ("m" paste-primary-selection)
          ("M" yank-at-point))))
  ;; Goodies
  (ryo-modal-keys
    ("o" (("k" (("u" kill-buffer)
                ("n" switch-to-last-buffer)
                ("c" kill-current-buffer)
                ("s" save-buffer :norepeat t)
                ("l" create-scratch-buffer)
                ("x" reopen-killed-file)
                ("X" reopen-killed-file-fancy)
                ) :name "Buffers, Files & and M-x")
          ("a" (("t" ignore :read t :name "insert text")
                ("c" comment-dwim :name "Comment")
                ("i" string-rectangle)) :name "Insert")
          ("w" (("r" split-window-below)
                ("s" split-window-right)
                ("d" delete-window)
                ("a" delete-other-windows)
                ("t" make-frame)) :name "Windows"))))
  ;; digit-arguments
  (ryo-modal-keys
    ;; first argument to ryo-modal-keys may be a list of keywords.
    ;; these keywords will be applied to all keybindings.
    (:norepeat t)
    ("0" "M-0")
    ("1" "M-1")
    ("2" "M-2")
    ("3" "M-3")
    ("4" "M-4")
    ("5" "M-5")
    ("6" "M-6")
    ("7" "M-7")
    ("8" "M-8")
    ("9" "M-9"))
  ;; Region
  (ryo-modal-keys
    (:norepeat t)
    ("v" set-mark-command)
    ("V" (("v" rectangle-mark-mode)
          ("c" exchange-point-and-mark))))
  ;; emacs-lisp
  (ryo-modal-major-mode-keys
    'emacs-lisp-mode
    ("s" (("s" eval-buffer)
          ("r" eval-region)
          ("f" eval-defun)))))

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
  "M-f")
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
 "M-<return>" 'new-line-dwim
 "M-e" 'hippie-expand
 "M-n" 'dabbrev-expand
 "C-c s u" 'straight-use-package
 "C-c s g" 'straight-get-recipe
 "C-;" 'iedit-mode
 "C-x C-y" 'pp-macroexpand-last-sexp)

;;** dired-mode-map
(general-define-key
 :keymaps 'dired-mode-map
 "RET" 'dired-find-alternate-file
 "M-RET" 'dired-find-file)
;;** centered-cursor-keymap
;; (general-define-key
;;  :keymaps 'centered-cursor-keymap
;;  "C-M--" 'centered-cursor-raise-position-manually
;;  "C-M-+" 'centered-cursor-lower-position-manually
;;  "C-M-=" 'centered-cursor-lower-position-manually
;;  "C-M-0" 'centered-cursor-reset-position-manually)
;;** key-translation-map
(general-define-key
 :keymaps 'key-translation-map
 "C-p" "C-u"
 "C-u" "C-p")
;; (define-key key-translation-map (kbd "C-p") (kbd "C-u"))
;; (define-key key-translation-map (kbd "C-u") (kbd "C-p"))
;;** smartparens-mode-map
(general-define-key
 :keymaps 'smartparens-mode-map
 "M-(" 'sp-wrap-round
 "M-s" 'sp-splice-sexp
 "M-r" 'sp-splice-sexp-killing-around)

(general-define-key
   :keymaps 'override
   "S-SPC" 'ryo-modal-global-mode
   "C-t" 'ryo-modal-global-mode)
;;** lispy-mode-map
(general-define-key
 :keymaps 'lispy-mode-map
 "e" 'special-lispy-different)
;;** python-mode-map
(general-define-key
 :keymaps 'python-mode-map
 "C-c C-d" nil)

;;* Tree sitter grammar
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
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
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (vue . ("https://github.com/ikatyang/tree-sitter-vue"))
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;;* Set the font
(custom-set-faces
 `(default ((t (:family "Fantasque Sans Mono"
                        :foundry "outline"
                        :slant normal
                        :weight normal
                        :height 130
                        ;; :height ,(let ((h (display-pixel-width)))
                        ;;            (cond
                        ;;             ((>= 1280 h) 130)
                        ;;             ((< 1280 h) 110)))
                        :width normal)))))
;;* Emacs theme


(use-package idea-darkula-theme
  :init
  (setq custom--inhibit-theme-enable nil)
  ;; (setq custom--inhibit-theme-enable 'apply-only-user)
  :config
  (push (substitute-in-file-name "~/.emacs.d/idea-darkula-theme/") custom-theme-load-path)
  (load-theme 'idea-darkula t)
  (let ((class '((class color) (min-colors 89)))
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
     '(font-lock-type-face ((t (:foreground "#A8B5C3"))))
     ;; End of dosen't work for some reason
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


;;; Failed experiment
  ;; (push (substitute-in-file-name "~/.emacs.d/elisp/") custom-theme-load-path)
  ;; (load-theme 'custom-idea-darkula t)
  )

(use-package dracula-theme
  :disabled
  :no-require
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
(use-package hydra
  ;;; Not in use
  ;; :config
  ;; (ryo-modal-key
  ;;  "q f" :hydra
  ;;  '(hydra-fastmoving ()
  ;;                     "Generic fast moving"
  ;;                     ("n" scroll-up)
  ;;                     ("u" scroll-down)
  ;;                     ("U" ccm-scroll-down)
  ;;                     ("N" ccm-scroll-up)
  ;;                     ("]" forward-paragraph)
  ;;                     ("[" backward-paragraph)
  ;;                     ("q" nil "cancel" :color blue)))
  ;; (ryo-modal-key
  ;;  "q i" :hydra
  ;;  '(hydra-indent ()
  ;;                 "Indent Mode"
  ;;                 ("n" mymy/elpy-nav-move-line-or-region-down)
  ;;                 ("u" mymy/elpy-nav-move-line-or-region-up)
  ;;                 ("N" shift-left)
  ;;                 ("U" shift-right)
  ;;                 ("q" nil "cancel" :color blue)))
  )

(use-package electric-operator)
(use-package highlight-indentation :config (highlight-indentation-mode 1))
(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))

(use-package ag)
;; sp-pair is not suitiable when you have strict-mode activate
(use-package wrap-region
  ;; Shouldn't be using this
  :disabled
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
  (emacs-lisp-mode . aggressive-indent-mode))

(use-package goto-chg
  :config
  (ryo-modal-keys
    ("g;" goto-last-change)
    ("G:" goto-last-change-reverse)))

;;** Web programming
;; Not using for now
;; (use-package web-mode)
;; (use-package impatient-mode)

;; (use-package centered-cursor-mode
;;   :straight (:type git :host github :repo "andre-r/centered-cursor-mode.el" :branch "dev")
;;   :config
;;   ;; (global-centered-cursor-mode)
;;   (setq scroll-preserve-screen-position t
;;         scroll-conservatively 0
;;         maximum-scroll-margin 0.5
;;         scroll-margin 99999)
;;   (setq scroll-preserve-screen-position nil
;;         scroll-conservatively 25
;;         ;; maximum-scroll-margin 0.25
;;         maximum-scroll-margin 0.25
;;         scroll-margin 0))

(use-package smartparens
  :init
  (defun mymy/smartparens-hook ()
    (smartparens-global-mode)
    (show-smartparens-global-mode))
  (setq mymy-lisp-modes '(emacs-lisp-mode clojure-mode cider-mode slime-mode lisp-mode))
  :config (sp-local-pair mymy-lisp-modes "'" "'" :actions nil)
  :hook ((after-init . mymy/smartparens-hook)
         (prog-mode . smartparens-strict-mode)))

(when (version< emacs-version "29")
  (use-package explain-pause-mode
    :straight (explain-pause-mode :type git :host github :repo "lastquestion/explain-pause-mode")
    :config
    (explain-pause-mode)))

(use-package golden-ratio
  :disabled
  :config (golden-ratio-mode t)
  (add-to-list 'golden-ratio-extra-commands 'ace-window)
  (ryo-modal-key "gr" #'golden-ratio-mode))

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
;; (use-package pdf-tools
;;   :defer 5
;;   :ryo
;;   (:mode 'pdf-view-mode)
;;   ("n" pdf-view-next-line-or-next-page)
;;   ("u" pdf-view-previous-line-or-previous-page)
;;   ("N" pdf-view-next-page)
;;   ("U" pdf-view-previous-page)
;;   :bind (:map pdf-view-mode-map ("C-x l" . pdf-get-page))
;;   :init
;;   (defun pdf-get-page ()
;;     (interactive)
;;     (message (concat "Page: " (int-to-string (image-mode-window-get 'page)))))
;;   ;; Don't make the daemon fail when a new version is avalible
;;   (ignore-errors (pdf-tools-install))
;;   :config
;;   ;; It simply needs to exist to extend saveplace
;;   (use-package saveplace-pdf-view)
;;   :hook
;;   (pdf-view-mode . pdf-isearch-minor-mode))

;; (use-package org-noter
;;   ;; Someday i will integrate this with org roam or not
;;   :disabled
;;   :config
;;   (setq org-noter-notes-window-location 'other-frame)
;;   (setq org-noter-always-create-frame nil)
;;   (setq org-noter-notes-search-path '("~/Sync/Notes/"))
;;   (setq org-noter-default-notes-file-names nil)
;;   (ryo-modal-major-mode-keys
;;     'pdf-view-mode
;;     ("i" org-noter-insert-note)
;;     ("I" org-noter-insert-precise-note)))
;;** Vterm
;; (use-package vterm
;;   ;; Git: Use vterm
;;   ;; Comment: now that I use java, vterm is more comfortable than
;;   ;; opening a terminal
;;   ;; ;; I prefer to open a terminal before opening this
;;   ;; ;; Good package though
;;   ;; :disabled
;;   ;; :bind
;;   ;; ;; ("C-x l" . vterm)
;;   :config
;;   (defun mymy/vterm-hook ()
;;     (centered-cursor-mode -1))
;;   :hook
;;   ((vterm-mode . mymy/vterm-hook)))

;;** calibredb
;; (use-package calibredb
;;   :defer 5
;;   :config
;;   (defun mymy-calibredb-update-list ()
;;     (interactive)
;;     (setq calibredb-search-entries (calibredb-candidates))
;;     (setq calibredb-full-entries calibredb-search-entries))
;;   (setq calibredb-root-dir "~/Sync/CalibreLibrary/")
;;   (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
;;   (setq calibredb-library-alist '(("~/Sync/")))
;;   )

;;** Spreedsheet
;; Will keep this two just in case
(use-package rainbow-numbers-mode
  :disabled
  :straight nil
  :load-path "~/.emacs.d/elisp/rainbow-numbers-mode.el")
;; (use-package ses-mode
;;   :no-require
;;   :straight nil
;;   :config
;;   (setq ses-after-entry-functions '(next-line))
;;   :hook
;;   (ses-mode . rainbow-numbers-mode))
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
  (defun tmp-lispy-fix ()
    (interactive)
    (if ryo-modal-global-mode
        (progn
          (ryo-modal-global-mode 0)
          (lispy-mode 1))
      (ryo-modal-global-mode 1)
      (lispy-mode -1)))
  (ryo-modal-major-mode-keys
    'clojure-mode
    ("S-SPC" tmp-lispy-fix)
    ("C-t" tmp-lispy-fix))
  (ryo-modal-major-mode-keys
    'lisp-mode
    ("S-SPC" tmp-lispy-fix)
    ("C-t" tmp-lispy-fix))
  :hook
  (clojure-mode . lispy-mode)
  (emacs-lisp-mode . lispy-mode)
  (lisp-mode . lispy-mode))

(use-package clj-refactor
  :straight clj-refactor pkg-info
  :config
  (cljr-add-keybindings-with-prefix "C-c m")
  :hook
  (cider-mode . clj-refactor-mode))

(use-package flycheck-clj-kondo)

(use-package cider
  :defer 5
  :config
  (defun cider-find-and-clear-repl-buffer ()
    (interactive)
    (cider-find-and-clear-repl-output t))
  (defun cider-pprint-eval-defun-at-point-to-comment ()
    (interactive)
    (cider-pprint-eval-defun-at-point t))
  :config
  (setq cider-test-show-report-on-success t)
  (ryo-modal-major-mode-keys
    'clojure-mode
    ("r" (("r" cider-run)
          ("f" cider-pprint-eval-defun-at-point)
          ("c" cider-find-and-clear-repl-output)
          ("C" cider-find-and-clear-repl-buffer)
          ("p" cider-eval-defun-to-comment)
          ("P" cider-pprint-eval-defun-at-point-to-comment))))
  (with-eval-after-load 'outline
    (ryo-modal-major-mode-keys
      'clojure-mode
      ("M-RET" outline-insert-heading)
      ("M-N" outline-promote)
      ("M-U" outline-demote)
      ("M-e" outline-toggle-children)))
  ;; Following three lines are for java interop documentation
  (setq cider-enrich-classpath t)
  (cider-add-to-alist 'cider-jack-in-dependencies "mx.cider/enrich-classpath" "1.9.0")
  (cider-add-to-alist 'cider-jack-in-dependencies "mx.cider/tools.deps.enrich-classpath" "1.9.0")
  :hook
  (clojure-mode . cider-mode)
  (cider-repl-mode . aggressive-indent-mode)
  (cider-repl-mode . subword-mode)
  (cider-repl-mode . cider-company-enable-fuzzy-completion)
  (cider-mode . cider-company-enable-fuzzy-completion))

(use-package clojure-mode
  :defer 5
  :config
  (require 'flycheck-clj-kondo)
  (defun mymy/clojure-mode-hook ()
    (flycheck-mode)
    ;; Set before enabling lsp
    (setq-local lsp-enable-indentation nil)
    (lsp)
    (aggressive-indent-mode)
    )
  (add-hook 'clojure-mode-hook #'mymy/clojure-mode-hook)
  )

;; jsonrpc has to be put before eglot because it eglot will try to do
;; something that will create dummy functions and for some reason that
;; doesn't allow me to overwrite over them.
;; (use-package jsonrpc)
;; (use-package eglot
;;   :config
;;   (add-to-list 'eglot-server-programs
;;                `(csharp-mode . ("omnisharp" "-lsp")))
;;   )

;;** SQL

(use-package ejc-sql
  :config
  (setq clomacs-httpd-default-port 8090) ; Use a port other than 8080.
  (setq ejc-result-table-impl 'orgtbl-mode)

  (add-hook 'ejc-sql-minor-mode-hook
            (lambda ()
              (ejc-eldoc-setup)))

  (setq ejc-connections
         '(("Asig2"
            (:classname . "com.mysql.jdbc.Driver")
            (:classpath .
                        ["/home/cheerio-pixel/.m2/repository/mysql/mysql-connector-java/5.1.44/mysql-connector-java-5.1.44.jar"])
            (:password . "testtest")
            (:user . "MySQL test")
            (:port . "3306")
            (:host . "localhost")
            (:dbname . "Asig2")
            (:dbtype . "mysql"))
           ("mysql1"
            (:classname . "com.mysql.jdbc.Driver")
            (:classpath .
                        ["/home/cheerio-pixel/.m2/repository/mysql/mysql-connector-java/5.1.44/mysql-connector-java-5.1.44.jar"])
            (:password . "testtest")
            (:user . "MySQL test")
            (:port . "3306")
            (:host . "localhost")
            (:dbname . "intro2")
            (:dbtype . "mysql"))
           ("lab3"
            (:classname . "com.mysql.jdbc.Driver")
            (:classpath .
                        ["/home/cheerio-pixel/.m2/repository/mysql/mysql-connector-java/5.1.44/mysql-connector-java-5.1.44.jar"])
            (:password . "testtest")
            (:user . "MySQL test")
            (:port . "3306")
            (:host . "localhost")
            (:dbname . "lab3")
            (:dbtype . "mysql"))
           ("Group"
            (:classname . "com.mysql.jdbc.Driver")
            (:classpath .
                        ["/home/cheerio-pixel/.m2/repository/mysql/mysql-connector-java/5.1.44/mysql-connector-java-5.1.44.jar"])
            (:password . "testtest")
            (:user . "MySQL test")
            (:port . "3306")
            (:host . "localhost")
            (:dbname . "Tarea2Group")
            (:dbtype . "mysql"))
           ("Final"
            (:classname . "com.mysql.jdbc.Driver")
            (:classpath .
                        ["/home/cheerio-pixel/.m2/repository/mysql/mysql-connector-java/5.1.44/mysql-connector-java-5.1.44.jar"])
            (:password . "testtest")
            (:user . "MySQL test")
            (:port . "3306")
            (:host . "localhost")
            (:dbname . "ProyectoFinal")
            (:dbtype . "mysql"))
           ))

  (with-eval-after-load 'company
    (require 'ejc-company)
    (push 'ejc-company-backend company-backends)
    (add-hook 'ejc-sql-minor-mode-hook
              (lambda ()
                (company-mode t)))
    (setq ejc-complete-on-dot t)
    )
  )

(use-package tree-sitter
  :disabled
  :straight tree-sitter-langs
  :config
  (require 'tsc)
  (tree-sitter-require 'python)
  (tree-sitter-require 'c-sharp)
  )

;; (use-package dyalog-mode
;;   :disabled
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.apl\\'" . dyalog-mode))
;;   (add-to-list 'auto-mode-alist '("\\.dyalog$" . dyalog-mode)))

;;** CSharp

(use-package csharp-mode
  :disabled
  :if (version< emacs-version "29")
  :config
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

  ;; (setq c-default-style (delete '(csharp-mode . "csharp") c-default-style))
  ;; (add-to-list 'c-default-style '(csharp-mode . "bsd"))

  (with-eval-after-load 'lsp-mode
    (add-hook 'csharp-mode-hook #'lsp)
    )
  ;; (with-eval-after-load 'aggressive-indent
  ;;   (add-hook 'csharp-mode-hook #'aggressive-indent-mode)
  ;;   )
  (with-eval-after-load 'csharp-mode
    (add-hook 'csharp-mode-hook #'lsp)
    )
  (with-eval-after-load 'dap-mode
    ;; Enabling only some features
    ;; (setq dap-auto-configure-features '(sessions locals controls tooltip))
    (require 'dap-netcore)
    ;; (dap-netcore--debugger-install)
    (setq dap-netcore-download-url "https://github.com/Samsung/netcoredbg/releases/download/2.2.0-974/netcoredbg-linux-amd64.tar.gz")
    (dap-register-debug-template
     "NetCoreDbg::Launch (MyMy3)"
     (list :type "coreclr"
           :request "launch"
           :mode "launch"
           :name "NetCoreDbg::Launch"
           :console "externalTerminal"
           :dap-compilation "dotnet build"))
    )
  (with-eval-after-load 'yassnippet
    (yas-define-snippets
     'csharp-mode
     '(;; "Key" "Template" "Name"
       ("interface" "interface I${1:object} {
    void ${2:sampleMethod}(var ${3:sampleParameter});
    int ${4:sampleVariable} { get; set;}
}" "interface")

       ("d" "/// <summary>
/// $1
/// </summary>" "/// <summary> ... </summary>")
       ("d" "/// <param name=\"$1\">$2</param>" "/// <param name=\"....\">...</param>")
       ("d" "/// <returns>$1</returns>" "/// <returns> .. </returns>")
       ("d" "/// <exception cref=\"$1\">$2</exception>" "/// <exception cref=\"...\">...</exception>")
       ;; ("up" "(use-package ${1:package-name}$0)" "use-package")
       ))
    )
  )

;;** lsp
(use-package dap-mode
  :disabled
  :straight (:host github :repo "emacs-lsp/dap-mode" :files ("*.el"))
  :config
  (require 'dap-ui)

  (setq dap-internal-terminal #'dap-internal-terminal-vterm)

  (el-patch-defun dap-debug (debug-args)
    "Run debug configuration DEBUG-ARGS.

If DEBUG-ARGS is not specified the configuration is generated
after selecting configuration template.

:dap-compilation specifies a shell command to be run using
`compilation-start' before starting the debug session. It could
be used to compile the project, spin up docker, ...."
    (interactive (list (-> (dap--completing-read "Select configuration template: "
                                                 (-mapcat #'funcall dap-launch-configuration-providers)
                                                 'cl-first nil t)
                           cl-rest
                           copy-tree)))
    ;; NOTE: the launch configuration must be expanded *before* being passed to a
    ;; debug provider. This is because some debug providers (e.g. dap-python) pass
    ;; some fields of DEBUG-ARGS as shell arguments in :program-to-launch and try
    ;; very hard to quote them. Because of this, `dap-start-debugging' cannot
    ;; expand them properly. Any python configuration that uses variables in :args
    ;; will fail.
    (let* ((debug-args (dap-variables-expand-in-launch-configuration debug-args))
           (taskConfigurations (dap-tasks-configuration-get-all))
           (launch-args (or (-some-> (plist-get debug-args :type)
                              (gethash dap--debug-providers)
                              (funcall debug-args))
                            (user-error "Have you loaded the `%s' specific dap package?"
                                        (or (plist-get debug-args :type)
                                            (user-error "%s does not specify :type" debug-args)))))
           (cb (lambda ()
                 (if (functionp launch-args)
                     (funcall launch-args #'dap-start-debugging-noexpand)
                   (dap-start-debugging-noexpand launch-args)))))
      (-if-let ((&plist :dap-compilation) launch-args)
          ;;                                      MAY I ASK WHY THIS HAPPENS TO ME?
          (dap-debug-run-task (el-patch-wrap 1 0 (list `(:cwd ,(or (plist-get launch-args :dap-compilation-dir)
                                                                   (lsp-workspace-root)
                                                                   default-directory)
                                                              :command ,dap-compilation
                                                              :label ,(truncate-string-to-width dap-compilation 20)))) cb)
        (-if-let ((&plist :preLaunchTask) launch-args)
            (let* ((task (dap-tasks-get-configuration-by-label preLaunchTask))
                   (tasks (dap-tasks-configuration-get-depends task)))
              (if tasks
                  (dap-debug-run-task tasks cb)
                (user-error "No valid tasks found labelled \"%s\". Please check your tasks.json" preLaunchTask)))
          (funcall cb)))))
  )

(use-package lsp-mode
  ;; :defer 5
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c-mode . lsp)
         (c++-mode . lsp))
  :config
  ;; For debugging
  ;; (setq lsp-log-io t)
  ;; (setq lsp-log-io nil)
  ;; Please forgive my soul for being foolish
  ;; (setq lsp-keep-workspace-alive t)
  ;; (lsp-register-custom-settings '(("omnisharp.enableImportCompletion" t)))
  ;; (setq lsp-completion-provider :none)
  (setq lsp-completion-provider :none)
  ;; lsp-enable-snippet
  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/

  ;; And by default they have to put the most uncomfortable position.
  (setq lsp-lens-place-position 'above-line)
  (bind-key "M-RET" #'lsp-signature-activate 'lsp-mode-map)
  (with-eval-after-load 'which-key
    (lsp-enable-which-key-integration t))
  (with-eval-after-load 'csharp-mode
    (add-hook 'csharp-mode-hook #'lsp)
    )
  (with-eval-after-load 'vue-ts-mode
    (setq lsp-vetur-format-default-formatter-css "none")
    (setq lsp-vetur-format-default-formatter-html "none")
    (setq lsp-vetur-format-default-formatter-js "none")
    (setq lsp-vetur-validation-template nil)
    )
  (with-eval-after-load 'dap-mode
    ;; Enabling only some features
    ;; (setq dap-auto-configure-features '(sessions locals controls tooltip))
    (require 'dap-netcore)
    ;; (dap-netcore--debugger-install)
    (setq dap-netcore-download-url "https://github.com/Samsung/netcoredbg/releases/download/2.2.0-974/netcoredbg-linux-amd64.tar.gz")
    (dap-register-debug-template
     "NetCoreDbg::Launch (MyMy3)"
     (list :type "coreclr"
           :request "launch"
           :mode "launch"
           :name "NetCoreDbg::Launch"
           :console "externalTerminal"
           :dap-compilation "dotnet build"))
    )
  :hook
  (;; (lsp-mode . lsp-enable-which-key-integration)
   (lsp-mode . lsp-signature-mode)))

(use-package lsp-ui
  :config (setq lsp-ui-sideline-show-hover t
                lsp-ui-sideline-delay 0.5
                lsp-ui-doc-delay 0.5
                lsp-ui-sideline-ignore-duplicates t
                lsp-ui-doc-position 'bottom
                lsp-ui-doc-alignment 'frame
                lsp-ui-doc-header nil
                lsp-ui-doc-include-signature t
                lsp-ui-doc-use-childframe t)
  ;; lsp-doc-show
  :commands lsp-ui-mode)

(use-package lsp-pyright
  :after lsp-mode
  :config
  (add-hook 'python-mode-hook (lambda () (require 'lsp-pyright)))
  (add-hook 'python-mode-hook #'lsp))

(use-package lsp-haskell
  ;; :defer 5
  :after lsp-mode
  ;; Uncontable tales i have of how this monster have ruined my day, not
  ;; because of itself, but because Of how much ram it needs and how my
  ;; little School-gorverment-given computer hogs from the effort of
  ;; keeping this thing afloat
  :config
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp)
  )

(use-package lsp-java
  :after lsp-mode
  :init
  (defun my/java-mode-hook ()
    (setq-local lsp-enable-indentation nil)
    (lsp)
    )
  (add-hook 'java-mode-hook #'my/java-mode-hook)
  (setenv "JAVA_HOME" "/usr/lib/jvm/java-17-openjdk/")
  (setq lsp-java-java-path
        (if (getenv "JAVA_HOME")
            (concat (getenv "JAVA_HOME") "bin/java")
          "java"))

  (setq lsp-java-configuration-runtimes
        `[(:name "JavaSE-17"
                 :path ,(getenv "JAVA_HOME")
                 :default t)
          (:name "JavaSE-11"
                 :path "/usr/lib/jvm/java-11-openjdk")]
        )
  (setq lsp-java-imports-gradle-wrapper-checksums
        [(:sha256: "a8451eeda314d0568b5340498b36edf147a8f0d692c5ff58082d477abe9146e4"
                   :allowed: t)])
  )

;; (use-package groovy-emacs-mode

;; )

;;** Kotlin
;; (use-package kotlin-mode
;;   :config
;;   (with-eval-after-load 'lsp
;;     (add-hook 'kotlin-mode-hook 'lsp))
;;   )


(use-package vue-ts-mode
  :straight (:type git :host github :repo "8uff3r/vue-ts-mode")
  :config
  (add-hook 'vue-ts-mode-hook #'lsp))

(use-package prettier-js
  :after vue-ts-mode
  :config
  (setq prettier-js-args '("--parser vue")))


;;** Python
(use-package python
  :bind (:map python-mode-map
              (("C-c C-q" . jupyter-eval-buffer)
               ("C-c C-j" . jupyter-run-repl)
               ;; M-t
               ("M-e" . python-nav-forward-block)
               ))
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
                               'font-lock-number-face keep))))
  (with-eval-after-load 'dap-mode
    (require 'dap-python)
    (setq dap-python-debugger 'debugpy)

    (add-hook 'dap-stopped-hook
              (lambda (arg) (call-interactively #'dap-hydra)))
    )
  ;; Prefer the new treesit
  ;; (with-eval-after-load 'tree-sitter
  ;;   (add-hook 'python-mode-hook #'tree-sitter-mode)
  ;;   (add-hook 'python-mode-hook #'tree-sitter-hl-mode)
  ;;   )
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
  ;;; TODO: Need to solve the initial lag when opening a pyhton file.
  :config
  (setenv "WORKON_HOME" "~/.cache/pypoetry/virtualenvs/")
  display-buffer-alist
  (add-to-list 'display-buffer-alist
               '("\\*poetry\\*"
                 (display-buffer-no-window)
                 ))
  (general-define-key
   "C-c c" 'poetry
   )
  :hook
  (python-mode . poetry-tracking-mode))
;;** Haskell
(use-package haskell-mode)

;;** Corfu

(use-package corfu
  ;; Optional customizations
  :config
  (gsetq corfu-cycle t) ;; Enable cycling for `corfu-next/previous'
  ;; (gsetq corfu-auto t)  ;; Enable auto completion
  (gsetq corfu-separator ?\s) ;; Orderless field separator
  ;; (gsetq corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (gsetq corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (gsetq corfu-preview-current nil) ;; Disable current candidate preview
  ;; (gsetq corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (gsetq corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (gsetq corfu-scroll-margin 5) ;; Use scroll margin

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
  :after corfu
  :config
  (general-define-key
   :keymap 'org-mode-map
   "C-M-k" 'cape-file))

(use-package nerd-icons-corfu
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;;** Company
(use-package company
  :disabled
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
  ;; (setq company-quick-access-keys '("n" "e" "i" "o" "u" "y" "m" "k" "h" "l"))
  ;; M-<n> where is a element of company-quick-access-keys
  (setq company-show-quick-access t)
  (setq company-tooltip-align-annotations t)
  (setq company-frontends
        '(company-pseudo-tooltip-unless-just-one-frontend
          company-preview-if-just-one-frontend
          company-echo-metadata-frontend
          company-pseudo-tooltip-frontend))
  (general-define-key
   "C-M-n" 'company-capf
   "C-M-e" 'company-complete)

  (with-eval-after-load 'smartparens-mode
    (general-define-key
     :keymaps 'smartparens-mode-map
     "C-M-e" 'company-complete))

  (global-company-mode))

(use-package company-posframe
  :disabled
  :config
  (company-posframe-mode))


(use-package company-quickhelp
  :disabled
  ;; This has a problem with frames-only-mode, I think, since it tries
  ;; to open a new window, it opens a frame, which in turn loses focus
  ;; of the current frame
  :after frames-only-mode
  :if (not frames-only-mode)
  :config
  (general-define-key
   :keymaps 'company-active-map
   "C-c h" 'company-quickhelp-manual-begin)
  (unless frames-only-mode
    (company-quickhelp-mode))
  )

(use-package slime-company
  :after (slime company)
  :config (setq slime-company-completion 'fuzzy
                slime-company-after-completion 'slime-company-just-one-space))

;;** flycheck
(use-package flycheck
  :config
  (ryo-modal-keys
    (:norepeat t)
    ("!"
     (("c" flycheck-buffer :name "Check buffer")
      ("e" flycheck-explain-error-at-point :name "Explain error at point")
      ("l" flycheck-list-errors :name "List errors")
      ("x" flycheck-disable-checker :name "Disable checker")
      ("o" flycheck-mode :name "Toggle mode"))))
  (ryo-modal-key
   "q ; f" :hydra
   '(hydra-syntaxcheck ()
                       "SyntaxCheck Mode"
                       ("n" flycheck-next-error :name "Next error")
                       ("u" flycheck-previous-error :name "Previous error")
                       ("ev" flycheck-buffer)
                       ("q" nil "cancel" :color blue)))
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-u" . flycheck-previous-error)))
;;** Vertico

(use-package vertico
  :straight vertico-posframe
  :init
  (vertico-mode)
  :config
  (setq enable-recursive-minibuffers t)
  ;; For one week without this [2022-11-01 Tue]
  ;; (vertico-posframe-mode -1)
  ;; (vertico-posframe-cleanup)
  ;; Kind of buggy for long lines
  ;; (setq vertico-posframe-poshandler #'posframe-poshandler-frame-top-center)
  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-center)
  ;; (setq vertico-posframe-parameters
  ;;       '((left-fringe . 8)
  ;;         (right-fringe . 8)))
  (with-eval-after-load 'hotfuzz
    (hotfuzz-vertico-mode))
  )

(use-package consult
  :straight
  :straight consult-projectile
  :config
  (with-eval-after-load 'lsp-mode
    (use-package consult-lsp))
  (setq consult-fontify-max-size 1024)
  :config
  ;; From https://www.reddit.com/r/emacs/comments/re31i6/comment/ho7ctf3/
  (defun up-directory (arg)
    "Move up a directory (delete backwards to /)."
    (interactive "p")
    (if (string-match-p "/." (minibuffer-contents))
        ;; zap-up-to-char but deletes the region instead of killing it.
        ;; This addition was by me.
        (let ((char ?/)
              (arg (- arg)))
          (let ((direction (if (>= arg 0) 1 -1)))
            (delete-region (point)
                           (progn
                             (forward-char direction)
                             (unwind-protect
                                 (search-forward (char-to-string char) nil nil arg)
                               (backward-char direction))
                             (point)))))
      (delete-minibuffer-contents)))

  (general-define-key
   :keymaps 'vertico-map
   "C-l" 'up-directory)

  (defun mymy-consult-line-using-region (start end)
    (interactive "r")
    (consult-line (when (region-active-p) (buffer-substring-no-properties start end))
                  (not (not current-prefix-arg))))

  (defun mymy-consult-line-using-thing-at-point ()
    (interactive)
    (consult-line (thing-at-point 'symbol)
                  (not (not current-prefix-arg))))

  (defun mymy-consult-line-cycle ()
    (interactive)
    (let ((vertico-cycle t))
      (call-interactively #'consult-line)))

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
   "C-c t h" 'consult-projectile)

  :config
  (ryo-modal-keys
    ;; ("F" mymy-consult-line-using-thing-at-point)
    ;; ("F" mymy-consult-line-using-region)
    ;; ("F" mymy-consult-line-cycle)
    ("F" consult-line)
    ;; Remember that ! SPC negates the focus
    ("f" consult-keep-lines)
    ("B" consult-mark)
    ("b" consult-outline)
    ("Yu" consult-yank-from-kill-ring)
    ;; Doesn't change selected to the current one
    ;; ("Yu" consult-yank-pop)
    ("ol" consult-locate)
    ("ot" (("t" consult-register)
           ("p" consult-bookmark)))
    ("ok" (;; ("e" helm-mini)
           ;; Offers something like the configuration of helm-mini
           ;; Recent files
           ("e" consult-buffer)
           ("f" consult-grep)
           ;; ("k" helm-complex-command-history)
           ("k" consult-complex-command)
           ("m" find-file :name "Find file")
           ("y" find-name-dired)
           ;; ("r" helm-find :name "Find file recursively") ;; Find files recursively
           ("r" consult-find :name "Find file recursively") ;; Find files recursively
           ;; ("i" helm-M-x)
           ("i" execute-extended-command)
           ;; ("o" helm-apropos)
           ;; ("o" apropos)
           ("o" describe-symbol)))))

(use-package consult-org-roam
  ;; For some reason, he preview started to move the point.
  :disabled
  :config
  ;; Why? Live previewing
  (consult-org-roam-mode))

;; Even though this package is only one commit and is from 2021/8/24,
;; I'm going to keep it since it's really simple.
(use-package consult-bibtex
  :straight (:type git :host github :repo "mohkale/consult-bibtex")
  :config
  (require 'consult-bibtex-embark)
  (ryo-modal-key "Seb" 'consult-bibtex)
  (setq consult-bibtex-default-action #'consult-bibtex-edit-notes)
  (define-key consult-bibtex-embark-map "RET" #'consult-bibtex)
  (with-eval-after-load 'embark
    (add-to-list 'embark-become-keymaps 'consult-bibtex-embark-map)))

(use-package marginalia
  :config
  ;; Until I find the way.
  ;;; I don't remember why I said the previous thing
  (marginalia-mode)
  )

(use-package embark
  ;; Unnecessary? Maybe, but this thing wasn't loading symlinking every .el file so I had to put it myself.
  :straight (:files ("*.el"))
  :config
  (general-define-key
   "C-," 'embark-act
   "M-," 'embark-dwim
   "C-c i" 'embark-act
   )
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (setq embark-verbose-indicator-display-action '(display-buffer-in-direction
                                                  (direction . top)))
  ;; (setq embark-verbose-indicator-display-action '(display-buffer-reuse-window))
  )

(use-package embark-consult
  ;; :no-require t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;; Good old fuzzy search
(use-package hotfuzz
  ;; I don't know what to do about you
  ;; I try to match "kill emacs" and you find nothing. But not only
  ;; that, you also have to not play nice with orderless.
  ;; Sorry, hotfuzz. I was supposed to use it without spaces. So
  ;; orderless gets activated if I start separating with spaces.
  ;; But this is still uncomfortable.
  :disabled
  :config
  (add-to-list 'completion-styles 'hotfuzz t)
  )

(use-package orderless
  ;; :config
  ;; ;; Put orderless at last since orderless put me things almost at random.
  ;; ;; (add-to-list 'completion-styles 'orderless t)
  ;; ;; (setq completion-styles '(basic partial-completion orderless))
  ;; (setq completion-styles '(orderless basic))
  ;; ;; matching characters in order, but non-consecutively
  ;; ;; (add-to-list 'orderless-matching-styles 'orderless-flex t)
  ;; (setq orderless-matching-styles '(;; orderless-literal
  ;;                                   orderless-regexp orderless-prefixes))
  ;; (setq orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex))
  ;; (setq completion-category-overrides '((file (styles basic partial-completion))))
  )

(use-package fussy
  :config
  ;; (push 'fussy completion-styles)
  (setq completion-styles '(basic partial-completion fussy))
  ;; Depending on the context, the normal one avoids throwing me
  ;; garbage
  ;; (gsetq fussy-filter-fn #'fussy-filter-orderless)
  (gsetq fussy-filter-fn #'fussy-filter-orderless-flex)

;;; Set to nil for fussy-score-threshold-to-filter-alist to take flace
  (gsetq fussy-score-threshold-to-filter 80) ;Stop throwing garbage

  ;; I know this is not taking effect
  (gsetq fussy-score-threshold-to-filter-alist
         '((flx-score . -100)
           (fussy-fuz-score . -100)
           (fussy-fuz-bin-score . -100)
           (fussy-fzf-native-score . 0)))

  ;; (general-remove-advice #'flx-score #'mymy-flx-ad)

  (setq
   ;; For example, project-find-file uses 'project-files which uses
   ;; substring completion by default. Set to nil to make sure it's using
   ;; flx.
   completion-category-defaults nil
   completion-category-overrides nil))

;;** Helm
(use-package helm
  :disabled
  :straight t
  :straight helm-swoop helm-projectile
  :diminish helm-mode
  :init
  (setq helm-split-window-default-side 'rigth)
  (setq helm-bookmark-show-location t)
  (setq helm-buffers-fuzzy-matching t)
  ;; TODO: Move this to org mode
  ;; (setq org-cycle-include-plain-lists 'integrate)
  (setq org-cycle-include-plain-lists t)
  (setq helm-mini-default-sources
        '(helm-source-buffers-list
          helm-source-recentf
          helm-source-buffer-not-found))
  (spaceline-helm-mode t)
  (spaceline-toggle-helm-number-on)
  (helm-mode)
  ;;*** Patches
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
  ;;*** Key bindings
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
  (ryo-modal-keys
    ;; ("b" isearch-backward)
    ("F" helm-swoop)
    ("B" helm-semantic-or-imenu)
    ("Yu" helm-show-kill-ring)
    ("il" helm-all-mark-rings)
    ("ol" helm-locate)
    ("ot" (("t" helm-register)
           ("p" helm-bookmarks)))
    ("ok" (("e" helm-mini)
           ("k" helm-complex-command-history)
           ("m" helm-find-files :name "Find file")
           ("y" find-name-dired)
           ("r" helm-find :name "Find file recursively") ;; Find files recursively
           ("i" helm-M-x)
           ("o" helm-apropos))))
  ;;*** Other packages
  ;; helm calibredb
  (with-eval-after-load 'calibredb
    (ryo-modal-key "Sb" 'calibredb-find-helm)
    (setq calibredb-helm-actions
          (helm-make-actions
           "Open file" 'calibredb-find-file
           "View details" 'calibredb-show-entry
           "Open file other frame" 'calibredb-find-file-other-frame
           "Open file with default tool" (lambda (candidate)
                                           (calibredb-open-file-with-default-tool nil candidate))
           "Open Cover Page" 'calibredb-find-cover
           "Set tags" 'calibredb-set-metadata--tags
           "Set comments" 'calibredb-set-metadata--comments
           "List fileds" 'calibredb-set-metadata--list-fields
           "Show metadata" 'calibredb-show-metadata
           "Export" 'calibredb-export
           "Remove" 'calibredb-remove
           "Insert an org link" (lambda (candidate)
                                  (unless (featurep 'org)
                                    (require 'org))
                                  (if (fboundp 'org-insert-link)
                                      (kill-new (format "[[%s][]]" (calibredb-getattr candidate :file-path)))))
           "Mail Add attachment" (lambda (candidate)
                                   (mail-add-attachment (calibredb-getattr candidate :file-path))))))

  ;; helm lsp mode
  (with-eval-after-load 'lsp-mode
    (use-package helm-lsp))

  ;; helm posframe
  (with-eval-after-load 'posframe
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
          (switch-to-minibuffer-window)))))

  ;; helm bibtex
  (with-eval-after-load 'bibtex
    (use-package helm-bibtex
      :config
      (ryo-modal-key "Seb" 'helm-bibtex)
      (el-patch-defvar helm-source-bibtex
        (helm-build-sync-source "BibTeX entries"
                                :header-name (lambda (name)
                                               (format "%s%s: " name (if helm-bibtex-local-bib " (local)" "")))
                                :candidates 'helm-bibtex-candidates
                                :filtered-candidate-transformer 'helm-bibtex-candidates-formatter
                                :action (helm-make-actions
                                         (el-patch-add "Edit notes"                 'helm-bibtex-edit-notes)
                                         "Open PDF, URL or DOI"       'helm-bibtex-open-any
                                         "Open URL or DOI in browser" 'helm-bibtex-open-url-or-doi
                                         "Insert citation"            'helm-bibtex-insert-citation
                                         "Insert reference"           'helm-bibtex-insert-reference
                                         "Insert BibTeX key"          'helm-bibtex-insert-key
                                         "Insert BibTeX entry"        'helm-bibtex-insert-bibtex
                                         "Attach PDF to email"        'helm-bibtex-add-PDF-attachment
                                         (el-patch-remove "Edit notes"                 'helm-bibtex-edit-notes)
                                         "Show entry"                 'helm-bibtex-show-entry
                                         "Add PDF to library"         'helm-bibtex-add-pdf-to-library))
        "Source for searching in BibTeX files.")))

  ;; helm org-roam
  (with-eval-after-load 'org-roam
    ;; (add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
    ;; (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags))
    ;; (add-to-list 'helm-completing-read-handlers-alist
    ;;              '(org-roam-node-insert . helm-completing-read-sync-default-handler))
    (add-to-list 'helm-completing-read-handlers-alist
                 '(org-roam-node-find . helm-completing-read-sync-default-handler))
    (add-to-list 'helm-completing-read-handlers-alist
                 '(mymy-org-roam-node-insert-wrapper . helm-completing-read-sync-default-handler)))

  ;; helm projectile
  (with-eval-after-load 'projectile
    (when helm-mode
      (setq projectile-switch-project-action 'helm-projectile)))

  ;; helm centaur tabs
  (with-eval-after-load 'centaur-tabs
    ;; (setq helm-source-centaur-tabs-group
    ;;       (helm-build-sync-source "Centaur tabs groups"
    ;;         :candidates #'centaur-tabs-get-groups
    ;;         :action '(("Switch to group" . centaur-tabs-switch-group))))
    ;; (add-to-list 'helm-mini-default-sources 'helm-source-centaur-tabs-group)
    )

  (with-eval-after-load 'org-ref
    (ryo-modal-key "Sei" #'org-ref-insert-cite-link))
  :hook
  (helm-minibuffer-set-up . helm-exchange-minibuffer-and-header-line))

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
(use-package magit :defer 5)

;;** Dired
(use-package dired+)

(use-package dired-subtree
  :bind (:map dired-mode-map
              ("i" . dired-subtree-toggle)))

(use-package dired-collapse
  :hook
  ((dired-mode . dired-collapse-mode)))

(use-package dirvish
  ;;; Why did I disable this?
  ;; :disabled
  :init
  (dirvish-override-dired-mode)
  :config
  (setq dirvish-preview-dispatchers (remove 'archive dirvish-preview-dispatchers))
  (setq dirvish-attributes '(file-size hl-line all-the-icons))
  ;; (setq dirvish-attributes nil)
  :bind
  (:map dired-mode-map ("C-l" . dired-up-directory)))

;; TODO: Put this in org roam section
(use-package git-auto-commit-mode
  :disabled
  )
;;** Ledger mode
(use-package ledger-mode :disabled :defer 5)
;;** Music
(use-package emms
  ;; Overkill, but extremely nice
  :disabled
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  (setq emms-source-file-default-directory "~/Music/"))
;; (use-package empv
;;   :straight (empv :type git :host github :repo "isamert/empv.el")
;;   :config
;;   (setq empv-base-directory (expand-file-name "~"))
;;   (defhydra hydra-empv-volume
;;     ()
;;     ("-" empv-volume-down)
;;     ("s" empv-volume-set)
;;     ("+" empv-volume-up)
;;     ("." nil :color blue))
;;   :bind
;;   ("C-c e n" . empv-playlist-next)
;;   ("C-c e p" . empv-playlist-prev)
;;   ("C-c e s" . empv-playlist-shuffle)
;;   ("C-c e e" . empv-toggle)
;;   ("C-c e l" . empv-playlist-loop-on)
;;   ("C-c e f" . empv-play-file)
;;   ("C-c e v" . hydra-empv-volume/body))

;;** Org mode
(use-package org
  :straight t
  :straight org-contrib
  :ryo
  (:mode 'org-agenda-mode)
  ("okc" org-agenda-exit)
  :init

  ;; Use timmestamps instead of UUID
  (gsetq org-id-method 'ts)

  ;; Default: "%Y%m%dT%H%M%S.%6N"
  (gsetq org-id-ts-format "%Y%m%dT%H%M%S.%6N")

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
     (java . t)))
  :config
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

  (ryo-modal-keys
    (:norepeat t)
    ("O" (("a" org-agenda)
          ("u" org-clock-goto))))
  (ryo-modal-key "Og" #'org-mark-ring-goto)

  (with-eval-after-load 'expand-region
    (ryo-modal-major-mode-keys
      'org-mode
      ("s" (("p" er/mark-org-element)
            ("T" er/mark-org-parent)))))
  (ryo-modal-major-mode-keys
    'org-mode
    ;; avalible r s
    ("M-m" org-meta-return)
    ("M-M" org-insert-heading-respect-content)
    ("M-c" org-toggle-checkbox)
    ("C-M-m" org-insert-heading-respect-content)
    ("M-U" org-metaright)
    ("M-N" org-metaleft)
    ;; Also works with a region
    ("Zv" org-fill-paragraph)
    ("m" org-return)
    ("r" (("s" org-preview-latex-fragment)
          ("t" org-latex-preview)
          ("l" org-toggle-link-display)
          ("o" org-open-at-point)
          ("i" +org-toggle-inline-image-at-point)))
    ("s" (;; ("a" org-agenda-archive-subtree)
          ("A" org-agenda-toggle-archive-tag)
          ;; ("t" outline-up-heading)
          ("t" mymy-org-back-to-heading)
          ("g" org-agenda-goto)
          ("ne" org-narrow-to-element)
          ("nv" narrow-to-region)
          ("nw" widen)
          ("m" outline-show-children)
          ("k" outline-show-branches)))
    ("Y" (("y" org-paste-special)))
    ("V" (("b" org-mark-subtree)))
    ("K" (("V" org-cut-special)
          ("W" org-copy-special)))
    ("a" org-beginning-of-line)
    ("e" org-end-of-line)
    ("O" (("o" org-clock-out)
          ("i" org-clock-in)
          ("r" org-refile)
          ("s" org-set-tags-command)
          ("e" org-agenda-clock-cancel)
          ("d" org-insert-drawer)
          ("c" org-columns)
          ("t" org-toggle-archive-tag)
          ("T" org-archive-subtree)
          ("w" (lambda () (interactive) (if org-timer-start-time
                                            (org-timer-stop) (org-timer-start)))
           :name "Toggle timer")
          ("ke" (lambda () (interactive) (org-set-property "TRIGGER" "chain-siblings(NEXT)")) :name "Make subtree trigger next")
          ("y" (lambda () (interactive) (org-latex-export-to-pdf nil t nil nil nil)) :name "Export subtree")))
    ("O R" :hydra
     '(hydra-org-refile ()
                        "Org refile"
                        ("r" (lambda () (interactive) (my/refile "notebook.org" "=What I'm Doing Now=")))
                        ("s" (lambda () (interactive) (my/refile "notebook.org" "=What I've Done Today=")))
                        ("t" (lambda () (interactive) (my/refile "notebook.org" "Completed task")))
                        ("n" next-line)
                        ("u" previous-line)
                        ("N" backward-char)
                        ("U" forward-char)
                        ("a" smarter-move-beginning-of-line)
                        ("e" end-of-line)
                        ("ga" beginning-of-buffer)
                        ("ge" end-of-buffer)
                        ("q" nil "cancel" :color blue)))
    ("q n" :hydra
     '(hydra-org-heading-move ()
                              "Heading move Mode"
                              ("n" org-forward-heading-same-level)
                              ("u" org-backward-heading-same-level)
                              ("U" org-metaup)
                              ("N" org-metadown)
                              ("q" nil "cancel" :color blue))))

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
  (setq org-refile-targets '((nil :maxlevel . 2)))
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
          ("=" (underline org-code))
          ("~" org-code verbatim)
          ("+" (:strike-through t))))
  ;;; Had this because I was really sensitive to info overload
  ;; (setq org-startup-folded t)
  (setq org-startup-folded "showeverything")
  (setq org-todo-keyword-faces
        '(("NEXT" . (:foreground "blue" :weight bold))
          ("TODO" . (:foreground "#F09432" :weight bold))
          ("CANCELLED" . (:foreground "red" :weight bold))
          ;; PROJect, as in something without a clear goal
          ;; ("PROJ" . (:foreground "white" :weight bold))
          ))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(s)" "CANCELLED(c)")
          ;; (type "PROJ(p)")
          ))

  (setq org-default-notes-file (f-join mymy-organization-system-directory "agenda.org"))
  ;; (org-capture-templates
  ;;  ;; https://orgmode.org/manual/Template-expansion.html#Template-expansion
  ;;  ;; https://orgmode.org/manual/Template-elements.html#Template-elements
  ;;  '(
  ;;    )
  ;;  )
  (setq org-enforce-todo-dependencies t)
  (setq org-tag-faces
        '(("Hold" (:foreground "yellow" :weight bold))
          ("Kobo" (:foreground "red" :weight bold))))
  (setq org-format-latex-options '(plist-put org-format-latex-options :scale 2.0 :background auto :foreground "white"))
  (setq org-highlight-latex-and-related '(latex script entities))
  (setq org-image-actual-width nil)
  (setq org-log-into-drawer t)
  :bind
  (("C-c o c" . org-capture)
   ;; :map org-mode-map
   ;; (("C-M-k" . company-files))
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
  :disabled
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
  :config
  (ryo-modal-major-mode-keys
    'org-agenda-mode
    ("n" org-agenda-next-line)
    ("u" org-agenda-previous-line)
    ("oks" org-agenda-write)
    ("rl" org-toggle-link-display)
    ("ro" org-agenda-open-link))
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
  (setq org-agenda-span 1)
  ;; (setq org-agenda-start-day "-1d")
  (setq org-agenda-start-day "1d")
  (setq org-super-agenda-groups
        '((:name "Doing" :todo "NEXT")
          (:name "Active Projects" :children "NEXT")
          (:name "Projects" :children todo)
          (:auto-priority)
          (:discard (:todo "DONE"))
          (:name "Stuck" :anything)
          ;; (:discard (:anything t))
          )))
;;; I STOPPED HERE
(use-package posframe)

(use-package which-key-posframe
  ;; Sometimes fires, sometimes doesn't and that is a problem
  :disabled
  :config
  (which-key-posframe-mode))

;; (use-package org-journal
;;   ;; Dailies is a better alternative
;;   :disabled
;;   ;; Why do i live, just to suffer?
;;   :init
;;   ;; Change default prefix key; needs to be set before loading org-journal
;;   (setq org-journal-prefix-key "C-c j")
;;   :config
;;   (setq org-journal-dir (concat main-dropbox-dir "journal/")
;;         org-journal-date-format "%A, %d %B %Y"))
(use-package org-superstar
  :config
  (org-superstar-configure-like-org-bullets)
  (setq org-superstar-special-todo-items t)
  (setq org-superstar-prettify-item-bullets nil)
  (setq org-superstar-headline-bullets-list '(?▹ ?⭆ ?○ ?✸ ?✿ ?✥ ?❂ ?❄)))

(use-package elisp-slime-nav
  :config
  ;; elisp-slime-nav-describe-elisp-thing-at-point C-c C-d C-d
  (if (s-suffix? "laptop" (system-name))
      (bind-key "C-." 'elisp-slime-nav-find-elisp-thing-at-point)
    (bind-key "M-." 'elisp-slime-nav-find-elisp-thing-at-point))
  :hook
  (emacs-lisp-mode . elisp-slime-nav-mode))

(use-package csv-mode
  :hook (csv-mode . csv-align-mode)
  :config
  (ryo-modal-major-mode-keys
    'csv-mode
    ("st" csv-align-fields)))

(use-package org-roam-ui
  :disabled
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
  :disabled
  :straight t
  :ryo
  ("Gt" git-timemachine-toggle)
  ("GT" hydra-git-timemachine/body)
  :bind (:map git-timemachine-mode-map
              ("n" . git-timemachine-show-next-revision)
              ("u" . git-timemachine-show-previous-revision))
  :hook
  ((git-timemachine-mode . hydra-git-timemachine/body))
  :config
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
            #'my-vulpea-insert-handle))

(use-package bibtex
  :config
  (setq bibtex-autokey-year-length 4))

(use-package org-ref
  :defer 5
  :config
  ;; (setq bibtex-completion-bibliography (list (concat main-dropbox-dir "Main.bib")))
  (setq bibtex-completion-bibliography `(,(concat dropbox-dir "My Library/MyLibrary.bib")))
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

  ;; Same function as ebib-notes-extract-org-ids but with roam_refs
  ;; This function is necessary to not slow down ebib
  (defun mymy-ebib-notes-extract-org-ids ()
    "Return a list of all Org ROAM_REFSs in the current buffer."
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
        (org-element-property :ROAM_REFS headline))))

  (setq ebib-notes-get-ids-function #'mymy-ebib-notes-extract-org-ids)
  :config
  (setq ebib-preload-bib-files `(,(concat dropbox-dir "My Library/MyLibrary.bib")))
  (setq ebib-bibtex-dialect 'biblatex)
  (setq ebib-file-associations '(("pdf" . "sioyek")
                                  ("ps" . "zathura")))
  (setq ebib-default-directory (concat dropbox-dir "My Library/"))
  (setq ebib-popup-entry-window t)
  ;; (setq ebib-layout 'index-only)
  (setq ebib-layout 'full)
  (setq ebib-reading-list-file (concat main-dropbox-dir "ReadingList.org"))

  ;; Ebib doesn't need to manage this
  (setq ebib-notes-directory org-roam-directory)
  (setq ebib-notes-storage 'multiple-notes-per-file)
  (setq ebib-notes-default-file (concat org-roam-directory "2022-01-09-12-38-23-zettelkasten.org"))
  ;; (setq ebib-notes-use-org-capture nil)
  (setq ebib-notes-use-org-capture "r")
  ;; Double %% for org expansions
  (setq ebib-notes-template "* lit %%<%%s>%%?\n:PROPERTIES:\n:ID:         %%(org-id-new)\n%K\n:END:\n%T\n\n\n")
  (add-to-list 'ebib-file-search-dirs "~/Attachments/bibliography/")
  (add-to-list 'org-capture-templates
               `("r" "bibliopraphic reference" entry
                 (file+olp ,ebib-notes-default-file "Zettelkästen" "lit notes")
                 #'ebib-notes-create-org-template))
  ;; controls what to do in org note
  ;; ebib-notes-open-note-after-hook
  :config
  ;; (add-to-list 'ryo-modal-excluded-modes 'ebib-index-mode)
  ;; (add-to-list 'ryo-modal-excluded-modes 'ebib-entry-mode)
  (ryo-modal-major-mode-keys 'ebib-index-mode
    ;; ("z" ebib-leave-ebib-windows)
    ;; ("Z" ebib-lower)
    ;; ("oks" ebib-save-current-database)
    ;; ("e" ebib-edit-entry)
    ;; ("E" end-of-line)
    ;; ("q" ebib-quit)
    ;; ("." ebib-quit)
    ("okc" ignore)
    )
  (ryo-modal-major-mode-keys 'ebib-entry-mode
    ;; ("e" ebib-edit-current-field)
    ;; ("E" end-of-line)
    ;; ("q" ebib-quit-entry-buffer)
    ;; ("." ebib-quit-entry-buffer)
    ("okc" ignore)
    )
  (general-define-key
   :keymaps 'ebib-index-mode-map
   "u" 'ebib-prev-entry
   "p" 'ebib-browse-url
   "q" 'ebib-quit
   "." 'ebib-quit
   "." 'ebib-index-c
   )
  (general-define-key
   :keymaps 'ebib-entry-mode-map
   "u" 'ebib-prev-field
   "p" 'ebib-browse-url
   "q" 'ebib-quit-entry-buffer
   "." 'ebib-quit-entry-buffer
   )
  :bind (("C-c n e" . ebib))
  )
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
  :after org-roam
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
  ;; Tested and works well, execept that it messes up location after refresh
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
  ;; Got solve
  ;; https://github.com/publicimageltd/lister/commit/f3e9748b3417184c36e301a381ec20ef4a88e511
  ;; (el-patch-defun delve--key--toggle-preview (zettel &optional prefix)
  ;;   "Toggle the display of the preview of ZETTEL.
  ;; With PREFIX, open ZETTEL's file in a buffer."
  ;;   (interactive (list (delve--current-item-or-error 'delve--zettel) current-prefix-arg))
  ;;   (if prefix
  ;;       (delve--key--open-zettel zettel)
  ;;     (let ((preview (and (not (delve--zettel-preview zettel))
  ;;                         (or (delve--get-preview-contents zettel)
  ;;                             "No preview available"))))
  ;;       (setf (delve--zettel-preview zettel) preview)
  ;;       (el-patch-swap
  ;;         (lister-refresh-at ewoc :point)
  ;;         (let ((ewoc lister-local-ewoc))
  ;;           (if (lister-with-sublist-below ewoc :point beg end
  ;;                 (lister--outline-invisible-p ewoc beg))
  ;;               (progn
  ;;                 (lister-mode-cycle-sublist ewoc :point)
  ;;                 (lister-refresh-at ewoc :point)
  ;;                 (lister-mode-cycle-sublist ewoc :point))
  ;;             (lister-refresh-at ewoc :point)))))))

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

  (defun mymy-delve-link-to-zettel (s)
    "Convert org id link to a delve--zettel object"
    (thread-last (plist-get (mymy-delve-parse-link s) :path)
                 (org-roam-node-from-id)
                 (delve--zettel-create)))

  ;; I putted myself the limitation of only modifying this function,
  ;; thinking that this would be good in the long term but man what a pain was this
  (el-patch-defun delve--key--yank (&optional arg)
    "Yank last kill, if it is a Delve token string; If it's a link,
    yank it as a node.
  Option ARG is currently ignored."
    (interactive)
    (ignore arg)
    (delve--assert-buf nil "This yank function has to be called in a Delve buffer")
    (let* ((yank (current-kill 0)))
      (if (el-patch-wrap 1 1
            (or (eq (car (get-text-property 0 'yank-handler yank))
                    'delve--yank-handler)
                ;; Check that yank is a link
                ;; Don't need to apply no-properties here because
                ;; string equal doesn't care about the text properties
                (when (string-equal (plist-get (mymy-delve-parse-link yank) :type)
                                    "id")
                  ;; Put yank such that it will pass all the checks objects have to pass
                  (thread-last (substring-no-properties yank)
                               (mymy-delve-link-to-zettel)
                               (delve-store--tokenize-object)
                               (list)
                               (format "%S")
                               (setq yank)))))
          (delve--yank-handler yank)
        (user-error "Current kill is not a Delve object; cannot yank"))))

  (defun mymy-delve-export--zettel-to-link (z &optional args)
    "Return zettel Z as an Org link pointing to its headline with the
format that i like.
Optional argument ARGS is ignored."
    (ignore args)
    (concat (org-link-make-string (concat "id:" (delve--zettel-id z))
                                  (delve--zettel-title z))
            ", "))

  ;; It broke for some reason
  (delve-export-new-backend 'mymy-yank-into-org
    "Print Delve zettels as links of the form that i use"
    :parent 'yank-into-org
    :separator "\n"
    :printers `((delve--pile    . ,(lambda (p o)
				     (concat
				      (string-join (--map (delve-export--item-string it o)
							  (delve--pile-zettels p))
						   ", ")
				      ",")))
		(delve--heading . ,(lambda (h _) (concat "- " (delve--heading-text h))))
		(delve--zettel  . ,#'mymy-delve-export--zettel-to-link)
		))

  (setq delve-export--yank-handlers (list 'mymy-yank-into-org))
  (el-patch-defvar delve-export--yank-handlers
    (list 'mymy-yank-into-org)
    "List of available backends for yanking (by name).
  When yanking, check which of these backends can be used in the
  current buffer by calling its `assert' function.  If there are
  multiple options available, let the user choose the proper
  backend.")

  ;; Work like find-file; untested
  (el-patch-defun delve-find-storage-file ((el-patch-add file-name))
    "Visit a Delve storage file or create a new Delve buffer.
    If the user selects a non-storage file, pass to `find-file'."
    (interactive (progn (delve--set-storage-dir) (expand-file-name (read-file-name "Find Delve storage or other file: " delve--last-storage-dir))))
    (el-patch-remove (delve--set-storage-dir))
    (el-patch-splice 2 0
      (let* ((file-name (expand-file-name (read-file-name "Find Delve storage or other file: " delve--last-storage-dir))))
        (pcase file-name
          ((pred delve--storage-p)
           (progn
             (switch-to-buffer (delve--read-storage-file file-name))
             ;; We set storage-dir here instead in the low level
             ;; functions, because else it would mess up the user's
             ;; workflow.
             (delve--set-storage-dir file-name)))
          ((pred delve--storage-file-name-p)
           (progn
             (switch-to-buffer (delve--new-buffer (file-name-base file-name)))
             ;; see above
             (delve--set-storage-dir file-name)))
          (_
           (find-file file-name))))))
  (defun mymy-delve--key--open-zettel (zettel &optional prefix)
    "A little function to do something before opening a zettel"
    (interactive (list (delve--current-item-or-error 'delve--zettel)))
    (delve--key--sync lister-local-ewoc '(4))
    ;; (delve--key--open-zettel zettel prefix)
    ;; Copied from org-roam-node-open. Omitted the part where it pushes the mark
    (let ((node (delve--zettel-node zettel)))
      (let ((m (org-roam-node-marker node))
            (cmd (or (cdr
                      (assq
                       (cdr (assq 'file org-link-frame-setup))
                       '((find-file . switch-to-buffer)
                         (find-file-other-window . switch-to-buffer-other-window)
                         (find-file-other-frame . switch-to-buffer-other-frame))))
                     'switch-to-buffer-other-window)))
        (if (not (equal (current-buffer) (marker-buffer m)))
            (funcall cmd (marker-buffer m)))
        (when (not (equal (org-roam-node-id node)
                          (org-roam-id-at-point)))
          (goto-char m))
        (move-marker m nil))
      (org-show-context)
      (when (> (org-outline-level) 0)
        (org-show-entry))))

  (defun mymy-delve--buffer-modified-p (&optional buffer tokens)
    (let* ((buf (or buffer (current-buffer)))
           (_ (delve--assert-buf buf))
           (l (or tokens (lister-map (lister-get-ewoc buf)  #'delve-store--tokenize-object)))
           (file-name (or (buffer-file-name buf) (error "This buffer is not associated with any file"))))
      (not (equal (delve-store--read file-name) l))))

  ;; Temporal functionality of backup
  ;; (progn
  ;;   (el-patch-defun delve--do-save-buffer (buf file-name)
  ;;     "Store the Delve list of BUF in FILE-NAME."
  ;;     ;; store list:
  ;;     (let ((l (lister-map (lister-get-ewoc buf)  #'delve-store--tokenize-object)))
  ;;       (unless (file-exists-p file-name)
  ;;         (make-empty-file file-name t))
  ;;       ;; Only save when modified
  ;;       (el-patch-wrap 1 0
  ;;         (when (el-patch-add (mymy-delve--buffer-modified-p buf l))
  ;;           (delve-store--write file-name l)
  ;;           ;; For git-auto-commit-mode
  ;;           (el-patch-add (run-hooks 'after-save-hook))
  ;;           ;; link buffer to file:
  ;;           (delve--setup-file-buffer buf file-name)))))

  ;;   (defun mymy-delve-hook ()
  ;;     ;; For some reason, this is not set
  ;;     (when-let ((buf (buffer-local-value 'delve-local-storage-file (current-buffer))))
  ;;       (setq buffer-file-name buf))
  ;;     (git-auto-commit-mode))

  ;;   (add-hook 'delve-mode-hook 'mymy-delve-hook))
  :config
  (setq delve-dashboard-tags '("entry"))
  (setq delve-display-path nil)
  (setq delve-storage-paths (concat main-dropbox-dir "Notes-Delve/"))
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
    ("d" delve--key--multi-delete)
    ("g" (lambda () (interactive) "Force update current delve-zettel" (delve--key--sync lister-local-ewoc '(4)))))
  (general-define-key
   :keymaps 'delve-mode-map
   "<tab>" 'ignore
   "C-<return>" 'mymy-delve--key--open-zettel
   "C-j" 'delve--key--tab
   )
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
  ;; There is a problem with the approach taken in the export backends
  ;; I think there should be two options one in which it takes by
  ;; defualt the first element of the list And another that prompts
  ;; for selection whenever there is a prefix
  ;; TODO: Implement prev paragraph as a patch
  ;; Maybe use let binding delve--key--yank for delve-export--yank-handlers
  ;; TODO: Function that provides fast access to enry notes
  ;; Like searching a heading "entries" or having a delve file with
  ;; all top entries being candidate for selection
  ;; Or just having one
  ;; TODO: Make function that undoes changes in delve buffer
  ;; TODO: Make function that opens current delve node in another collection
  ;; Limit only to zettels
  ;; This is the same a inspection
  ;; TODO: Implement undo functionality
  ;; To do this i need to first implement a way of getting the state,
  ;; a way of storing (Obviously a list but let's keep this in doubt)
  ;; and a way of restoring this state
  ;; Maybe i can save the ewoc
  ;; Maybe I can make undo work like a normal buffer
  ;; I will come back later, way later
  )

;;** Org roam
(use-package org-roam
  :after el-patch
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

  (defconst org-roam-db--table-schemata
    '((files
       [(file :unique :primary-key)
        title
        (hash :not-null)
        (atime :not-null)
        (mtime :not-null)])

      (nodes
       ([(id :not-null :primary-key)
         (file :not-null)
         (level :not-null)
         (pos :not-null)
         todo
         priority
         (scheduled text)
         (deadline text)
         title
         properties
         olp
         (el-patch-add backlinkcount)
         ]
        (:foreign-key [file] :references files [file] :on-delete :cascade)))
      (aliases
       ([(node-id :not-null)
         alias]
        (:foreign-key [node-id] :references nodes [id] :on-delete :cascade)))

      (citations
       ([(node-id :not-null)
         (cite-key :not-null)
         (pos :not-null)
         properties]
        (:foreign-key [node-id] :references nodes [id] :on-delete :cascade)))

      (refs
       ([(node-id :not-null)
         (ref :not-null)
         (type :not-null)]
        (:foreign-key [node-id] :references nodes [id] :on-delete :cascade)))

      (tags
       ([(node-id :not-null)
         tag]
        (:foreign-key [node-id] :references nodes [id] :on-delete :cascade)))

      (links
       ([(pos :not-null)
         (source :not-null)
         (dest :not-null)
         (type :not-null)
         (properties :not-null)]
        (:foreign-key [source] :references nodes [id] :on-delete :cascade)))))
  :config
  (setq org-agenda-files (list (concat mymy-org-roam-dir "Projects/20210715113548-projects.org")))
  (el-patch-defconst org-roam-db--table-schemata
    '((files
       [(file :unique :primary-key)
        title
        (hash :not-null)
        (atime :not-null)
        (mtime :not-null)])

      (nodes
       ([(id :not-null :primary-key)
         (file :not-null)
         (level :not-null)
         (pos :not-null)
         todo
         priority
         (scheduled text)
         (deadline text)
         title
         properties
         olp
         (el-patch-add backlinkcount)
         ]
        (:foreign-key [file] :references files [file] :on-delete :cascade)))
      (aliases
       ([(node-id :not-null)
         alias]
        (:foreign-key [node-id] :references nodes [id] :on-delete :cascade)))

      (citations
       ([(node-id :not-null)
         (cite-key :not-null)
         (pos :not-null)
         properties]
        (:foreign-key [node-id] :references nodes [id] :on-delete :cascade)))

      (refs
       ([(node-id :not-null)
         (ref :not-null)
         (type :not-null)]
        (:foreign-key [node-id] :references nodes [id] :on-delete :cascade)))

      (tags
       ([(node-id :not-null)
         tag]
        (:foreign-key [node-id] :references nodes [id] :on-delete :cascade)))

      (links
       ([(pos :not-null)
         (source :not-null)
         (dest :not-null)
         (type :not-null)
         (properties :not-null)]
        (:foreign-key [source] :references nodes [id] :on-delete :cascade)))))
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
  (defun mymy-org-roam-dailies-today (&optional goto)
    (interactive "P")
    (org-roam-dailies--capture (current-time) goto "d"))

  ;; Update after open capture
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

  ;; [BEGIN] Backlink count things
  (defun mymy-org-roam-get-backlinks (&optional id)
    (interactive)
    ;; If there is no entry, then that means we don't have any link
    (or (gethash (or id (org-id-get)) mymy-backlinks-count-cache)
        0))

  (defvar mymy-backlinks-count-cache (make-hash-table
                                      :test 'equal
                                      :size (caar (org-roam-db-query
                                                   [:select (funcall count id)
                                                    :from nodes])))
    "Hash table with all the pre-computed backlink count.")

  (defun mymy-update-backlinks-count-cache (cache)
    (--map
     (puthash (car it) (cadr it) cache)
     (org-roam-db-query
      [:select [links:dest (as (funcall count links:source) backlinkcount)]
       :from links
       :where (= links:type "id")
       :group :by links:dest
       :order :by backlinkcount :desc
       ])))

  (mymy-update-backlinks-count-cache mymy-backlinks-count-cache)

  (cl-defmethod org-roam-node-backlinkcount ((node org-roam-node))
    (format "[%d]" (mymy-org-roam-get-backlinks (org-roam-node-id node))))

  ;; [END]

  (setq mymy-org-roam-project-template '(("p" "project" plain "%?"
                                          :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: TODO\n\n* ${title}")
                                          :unnarrowed t)))
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

  ;; (benchmark-run 1000 (mymy-org-roam-get-link))
  ;; Benchmark 1000 repetitions
  ;; GC: 100000000
  ;; With org roam: 7.888778214
  ;; Without org roam: 2.5779902160000003
  ;; Didn't change because of the performance, only because of the
  ;; universality
  (defun mymy-org-roam-get-link (&optional arg)
    "Insert node at point as a link in the kill ring, with `C-u' copy
the name of the node"
    (interactive "P")
    (let* (;; (node (org-roam-node-at-point))
           ;; (node-id (org-roam-node-id node))
           ;; (node-name (org-roam-node-title node))
           (entry-id (org-entry-get nil "ID"))
           (node-id (or entry-id
                        (org-with-wide-buffer (goto-char (point-min)) (org-id-get))
                        (user-error "No id near.")))
           (entry-name (org-entry-get nil "ITEM"))
           (node-name (or (when entry-id
                            (when (string-empty-p entry-name) (user-error "Heading without a name"))
                            entry-name)
                          (cadar (org-collect-keywords '("TITLE")))
                          (user-error "No name."))))
      (kill-new
       (if (equal arg '(4))
           node-name
         ;; Old way
         ;; (format "[[id:%s][%s]]" node-id node-name)
         ;; New way
         ;; In this new way a link inside a link would not be copied
         ;; literally, only its description.
         (org-link-make-string
          (concat "id:" node-id)
          (org-link-display-format node-name))))))

  (defun mymy-org-roam-insert-next-heading (&optional arg)
    (interactive)
    (save-excursion
      (org-forward-heading-same-level (or arg 1))
      (mymy-org-roam-get-link))
    (yank))

  (defun mymy-org-id-get-create-with-roam-exclude (&optional force)
    (interactive)
    (org-entry-put (point) "ROAM_EXCLUDE" "t")
    (org-id-get-create force))

  ;; my org roam preview at point
  (defun mymy-org-id-open-without-push-mark (id)
    "Slight modification to org-id-open"
    (let ((m (org-id-find id 'marker)))
      (unless m
        (error "Cannot find entry with ID \"%s\"" id))
      (if (not (equal (current-buffer) (marker-buffer m)))
          (funcall 'switch-to-buffer (marker-buffer m)))
      (goto-char m)
      (move-marker m nil)
      (org-show-context)))

  (el-patch-defun org-roam-id-open (id (el-patch-swap _ arg))
    "Go to the entry with id ID.
Like `org-id-open', but additionally uses the Org-roam database."
    (org-mark-ring-push)
    (let ((m (el-patch-swap
               (or (org-roam-id-find id 'marker)
                   (org-id-find id 'marker))
               ;; org-id-find is more precise than the org-roam version
               ;; but the org roam version works always in the org roam dir.
               (or (org-id-find id 'marker)
                   (org-roam-id-find id 'marker))))
          cmd)
      (unless m
        (error "Cannot find entry with ID \"%s\"" id))
      ;; Use a buffer-switching command in analogy to finding files
      (setq cmd
            (or
             (cdr
              (assq
               (cdr (assq 'file (el-patch-swap ; For files
                                  org-link-frame-setup
                                  (if (equal arg '(4))
                                      org-link-frame-setup
                                    '((file . find-file))))))
               '((find-file . switch-to-buffer)
                 (find-file-other-window . switch-to-buffer-other-window)
                 (find-file-other-frame . switch-to-buffer-other-frame))))
             'switch-to-buffer-other-window))
      (if (el-patch-wrap 1 1            ; For headings
            (or (not (equal (current-buffer) (marker-buffer m)))
                (equal arg '(4))))
          (funcall cmd (marker-buffer m)))
      (goto-char m)
      (move-marker m nil)
      (org-show-context)
      (el-patch-add
        (when (org-current-level)
          (mymy-org-show-headline)))))

  ;; (el-patch-defun org-roam-id-find (id &optional markerp)
  ;;     "Return the location of the entry with the id ID using the Org-roam db.
  ;; The return value is a cons cell (file-name . position), or nil
  ;; if there is no entry with that ID.
  ;; With optional argument MARKERP, return the position as a new marker."
  ;;     (cond
  ;;      ((symbolp id) (setq id (symbol-name id)))
  ;;      ((numberp id) (setq id (number-to-string id))))
  ;;     (let ((node (org-roam-populate (org-roam-node-create :id id))))

  ;;       ;; Try to fix the inconsistency of position
  ;;       ;; The main problem is that the slot point is updated with the db
  ;;       (el-patch-add
  ;;         (let ((file (org-roam-node-file node)))
  ;;           (if (eq (buffer-file-name) file)
  ;;               (setf (org-roam-node-point node) (org-find-entry-with-id id))
  ;;             (with-current-buffer (or (find-buffer-visiting file)
  ;;                                      (find-file-noselect file))
  ;;               (setf (org-roam-node-point node) (org-find-entry-with-id id)))))
  ;;         )

  ;;       (when-let ((file (org-roam-node-file node)))
  ;;         (if markerp
  ;;             (unwind-protect
  ;;                 (let ((buffer (or (find-buffer-visiting file)
  ;;                                   (find-file-noselect file))))
  ;;                   (with-current-buffer buffer
  ;;                     (move-marker (make-marker) (org-roam-node-point node) buffer))))
  ;;           (cons (org-roam-node-file node)
  ;;                 (org-roam-node-point node))))))

  (defun mymy-org-roam-preview-node ()
    "Return a string representing the preview of node."
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
          (if (< 0 (caar (org-roam-db-query [:select level :from nodes :where (= id $s1)] id)))
              ;; Headline
              (save-window-excursion
                (mymy-org-id-open-without-push-mark id)
                (let* ((headline-context (org-element-context))
                       (beg (org-element-property :begin headline-context))
                       (end (org-element-property :end headline-context)))
                  (buffer-substring-no-properties beg end)))
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

  (defvar mymy-org-roam-preview-current-link nil
    "Link we are previewing.")

  (defun mymy-org-roam-show-preview ()
    (interactive)
    (when-let ((node-preview (mymy-org-roam-preview-node)))
      (when (string-empty-p node-preview)
        (error "Nothing to show"))
      (let ((buf (get-buffer-create "*mymy-org-roam-preview*")))
        (setq mymy-org-roam-preview-current-link (org-element-property :raw-link (org-element-context)))
        (with-current-buffer buf
          (org-mode)
          (erase-buffer)
          (insert node-preview)
          (goto-char (point-min))
          (mymy-org-show-headline)
          ;; (org-show-entry)
          ;; (org-show-all)
          )
        (posframe-show buf
                       :poshandler 'posframe-poshandler-frame-top-right-corner
                       :hidehandler 'mymy-posframe-hidehandler-org-roam-hide-preview
                       :border-width 1 :border-color "blue"))))

  (defun mymy-posframe-hidehandler-org-roam-hide-preview (info)
    (and (not (string-equal mymy-org-roam-preview-current-link
                            (org-element-property :raw-link (org-element-context))))
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
    (previous-line))

  (defun mymy-org-roam-propagate-state-to-link-inside-heading ()
    "Mark all links to headings with a TODO to current org-state"
    (save-excursion
      (org-back-to-heading t)
      (let ((title (org-element-property :raw-value (org-element-context)))
            link-list)
        ;; Set list of link's id in link-list
        (with-temp-buffer
          (insert title)
          (org-mode)
          (goto-char (point-min))

          ;; Get all the links in content
          (setq link-list
                (org-element-map (org-element-parse-buffer) 'link
                  (lambda (link)
                    (when (string= (org-element-property :type link) "id")
                      (org-element-property :path link))))))
        (when link-list
          (->> link-list
               ;; Get context from id
               (--map (save-window-excursion (mymy-org-id-open-without-push-mark it) (org-element-context)))
               ;; Only keep the headlines
               (--filter (eq 'headline (car it)))
               ;; Remove the headlines without TODO keyword
               (--remove (not (org-element-property :todo-keyword it)))
               ;; Finally, change the state of the headline.
               (--map (save-window-excursion
                        (mymy-org-id-open-without-push-mark (org-element-property :ID it))
                        (let ((org-log-done 'time))
                          (org-todo (substring-no-properties org-state))))))))))

  (add-hook 'org-after-todo-state-change-hook #'mymy-org-roam-propagate-state-to-link-inside-heading)

  ;; This was the intended behaviour, but I changed course and went
  ;; for a general implementation. I'm keeping this just in case.
  (defun mymy-org-roam-mark-done-inside-link ()
    "Mark all nodes in heading as done if current is marked as done"
    ;; Remember to compare using string= because it ignores the text
    ;; properties
    (when (string= "DONE" org-state)
      (mymy-org-roam-propagate-state-to-link-inside-heading)))

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

  (defun mymy-org-move-prev-heading ()
    (interactive)
    (mymy-org-move-next-heading -1))
  ;; (setq org-indirect-buffer-display 'dedicated-frame)
  (setq org-indirect-buffer-display 'new-frame)

  (defun mymy-org-id-roam-create ()
    (interactive)
    (org-id-get-create)
    (mymy-org-roam-update-headline))

  (defun mymy-org-roam-goto-index (&optional other-window force)
    "Go to index file
When interactive, FORCE is "
    (interactive (list current-prefix-arg nil))
    (org-roam-node-visit mymy-index-node other-window force))
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
  ;; (if (equal (car target) 'closure)
  ;;     (let* ((mode-name (ignore-errors (symbol-name mode)))
  ;;            (name (intern (concat "ryo" (and mode-name (format "-%s" mode-name)) (format "/lambda-%s" key)))))
  ;;       (defalias name target)
  ;;       (ryo-modal-key key name args))
  ;;   other-clause)
  (ryo-modal-keys
    ("S"
     (("l" org-roam-buffer-toggle)
      ("y"
       (("n" org-next-link)
        ("u" org-previous-link)
        ("a" mymy-org-roam-append-node-heading)
        ("k" mymy-org-roam-get-link)
        ("y" hydra-zettel/body)
        ("e" mymy-org-roam-insert-next-heading)
        ("b" (lambda () (interactive)
               (message (number-to-string (mymy-org-roam-get-backlinks))))
         :name "Say number of backlinks")))
      ("k"
       (("f" org-roam-dailies-goto-today)))
      ("n"
       (("n" org-roam-node-find)
        ("i" org-roam-node-insert)
        ("e" org-roam-node-insert-immediate)
        ("x" org-roam-extract-subtree)
        ("s" mymy-org-roam-goto-index)
        ;; ("b" move-to-notes)
        ))
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
        ;; Substitute for org-ref-insert-cite-link
        ("i" org-ref-insert-cite-link)
        ))
      ("o" mymy-org-id-roam-create)
      ("i"
       ;; (format-time-string "%c" (seconds-to-time 1645577664))
       (("i" mymy-insert-unix-date)
        ("e" (lambda (start end) (interactive "r")
               (thread-last (buffer-substring-no-properties start end)
                            ;; Returns 0 in case is not a number
                            (string-to-number)
                            (seconds-to-time)
                            (format-time-string "%c")
                            (message))))
        ("l" (lambda () (interactive)
               (let ((link (current-kill 0)))
                 (org-meta-return)
                 (mymy-insert-unix-date)
                 (end-of-line)
                 (newline)
                 (insert link)
                 (end-of-line)
                 (newline)
                 (org-meta-return)
                 (org-demote)))
         :name "Insert literature note"
         )
        ;; ("e" mymy-insert-exact-date-in-unix-as-link)
        ))
      ("r" mymy-refile-to-done))))
  (which-key-add-key-based-replacements
    "Sk" "Dailies"
    "Se" "Refs & bibtex"
    "Sn" "org-roam node"
    "St" "org-roam tags"
    )
  (setq org-roam-db-node-include-function (lambda () (not (member "ARCHIVE" (org-get-tags)))))
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
     ("c" "Current pending list" entry "* NEXT %T %?\n"
      :prepend t
      :target (file+olp ,(concat org-roam-directory "Projects/20210715113548-projects.org")
                        ("Projects" "Current pending list")))
     ("b" "Buffer" entry "* TODO Read [[%L][S]] %?\n"
      :prepend t
      :target (file+olp ,(concat org-roam-directory "Projects/20210715113548-projects.org")
                        ("Projects" "Stack")))
     ("c" "Current pending task" entry "* NEXT %T %?\n"
      :prepend t
      :target (file+olp ,(concat org-roam-directory "Projects/20210715113548-projects.org")
                        ("Projects" "Current pending list")))

     ;; New words
     ("e" "English" entry "* %?\n\n"
      :target (file+olp ,(concat org-roam-directory "2021-07-19-13-58-56-english.org")
                        ("Dictionary")))

     ;; Time Capsule
     ("f" "Future self" entry "* %?\n\n"
      :target (file+olp ,(concat org-roam-directory "2021-10-16-13-47-38-future_self.org")
                        ("Future self")))

     ;; Standard Operating Procedures
     ("o" "SOP" entry "* %?\n\n"
      :target (file+olp ,(concat org-roam-directory "2022-08-03-06-17-18-standard_operating_procedures.org")
                        ("Standard operating procedures")))
     ("d" "date" entry
      "* %<%H:%M> %?"
      :target (file+head+olp ,(concat org-roam-directory "dailies/" "%<%Y-%m-%d>.org")
                             "#+title: %<%Y-%m-%d>\n#+STARTUP: nofold\n"
                             ("%<%A, %d %B %Y>")
                             )
      :unnarrowed t)
     )
   )
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph)
   ("C-c n i" . org-roam-node-insert)
   ;; ("C-c n c" . org-roam-capture)
   ("C-c n c" . org-roam-dailies-capture-today)
   ;; Dailies
   ("C-c C-j C-j" . mymy-org-roam-dailies-today)
   ("C-c C-j j" . mymy-org-roam-dailies-today)
   :map org-mode-map
   ("C-c C-j" . nil)
   ("C-c C-j C-j" . mymy-org-roam-dailies-today)
   ("C-c C-j j" . mymy-org-roam-dailies-today)))

(use-package org-transclusion)

(use-package org-roam-bibtex
  :config
  (setq orb-roam-ref-format 'org-ref-v3)
  (org-roam-bibtex-mode)
  (setq orb-preformat-templates t)
  (setq orb-preformat-keywords '("title" "citekey" "entry-type" "date" "author"))
  )


(use-package asoc
  :straight (asoc :type git :host github :repo "troyp/asoc.el"))
(use-package doct
  ;; Description: doct is a function that provides an alternative,
  ;; declarative syntax for describing Org capture templates.
  )

(use-package org-capture-ref
  :defer 5
  :after org-roam
  ;; :straight (asoc.el :type git :host github :repo "troyp/asoc.el")
  :straight (org-capture-ref :type git :host github :repo "yantar92/org-capture-ref")
  :config
  (el-patch-defun org-capture-ref-clean-bibtex (string &optional no-hook)
    "Make sure that BiBTeX entry STRING is a valid BiBTeX.
Return the new entry string.

This runs `org-capture-ref-clean-bibtex-hook', unless NO-HOOK is non-nil."
    (with-temp-buffer
      (bibtex-mode)
      (bibtex-set-dialect (el-patch-swap 'BibTeX 'biblatex))
      (when string (insert string))
      (goto-char 1)
      (unless no-hook
        (run-hooks 'org-capture-ref-clean-bibtex-hook))
      (goto-char 1)
      (dolist (field (bibtex-parse-entry 'content))
        (pcase (intern (concat ":" (car field)))
          (':=type= (org-capture-ref-set-bibtex-field :type (cdr field)))
          (':=key= (org-capture-ref-set-bibtex-field :key (cdr field)))
          ;; Other fields may contain unwanted newlines.
          (key (org-capture-ref-set-bibtex-field key (replace-regexp-in-string "\n[ \t]*" " " (cdr field))))))
      (buffer-string)))

  (el-patch-defcustom org-capture-ref-default-bibtex-template (el-patch-concat "@${:type}{${:key},
      typealt     = {${:typealt}},
      author       = {${:author}},
      title        = {${:title}},
      journal      = {${:journal}},
      school      = {${:school}},
      volume       = {${:volume}},
      number       = {${:number}},
      pages        = {${:pages}},
      year         = {${:year}}," (el-patch-add "
      date         = {${:date}},") "
      doi          = {${:doi}},
      isbn          = {${:isbn}},
      url          = {${:url}},
      howpublished = {${:howpublished}},
      publisher = {${:publisher}},
      keywords     = {${:keywords}},
      note         = {Online; accessed ${:urldate}},
      created         = {${:created}},
      effort       = {${:effort}},
      link         = {${:link}},
      rss          = {${:rss}}
      }"
      )
    "Default template used to format BiBTeX entry.
If a keyword from the template is missing, it will remain empty."
    :type 'string
    :group 'org-capture-ref)

  ;; (org-capture-ref-generate-key-human-readable)
  ;; (mymy-org-capture-ref-generate-key)
  (defun mymy-org-capture-ref-generate-key ()
    ;; To avoid recursion
    ;; => org-capture-ref-format-bibtex
    ;; => org-capture-ref-clean-bibtex
    ;; => org-capture-ref-clean-bibtex-hook
    ;; => org-capture-ref-create-key-maybe
    ;; => org-capture-ref-generate-key
    (let ((bibtex-string (prog2 (unless (org-capture-ref-get-bibtex-field :key)
                                  (org-capture-ref-set-bibtex-field :key "placeholder"))
                             (org-capture-ref-format-bibtex)
                           (when (string= (org-capture-ref-get-bibtex-field :key) "placeholder")
                             (org-capture-ref-set-bibtex-field :key nil 'force)))))
      (with-temp-buffer
        (bibtex-mode)
        (bibtex-set-dialect 'biblatex)
        (insert bibtex-string)
        (goto-char 1)
        (cl-letf (((symbol-function 'bibtex-autokey-get-year) (condition-case err
                                                                  `(lambda () ,(bibtex-autokey-get-year))
                                                                (t (lambda () "")))))
          (bibtex-generate-autokey)))))

  (setq org-capture-ref-generate-key-functions '(mymy-org-capture-ref-generate-key))

  (defmacro mymy-org-capture-ref-set-bibtex-fields (&rest arg-list)
    "Set org-caputer-ref fields like setq"
    (declare (indent 0))
    (cons 'progn
          (cl-loop for i in arg-list
                   collect `(org-capture-ref-set-bibtex-field ,@i))))

  ;; After using, add the function to org-capture-ref-get-bibtex-functions
  (defmacro mymy-org-capture-ref-get-bibtex- (name link-match &rest body)
    "Create a site specific parser"
    (declare (indent defun))
    `(defun ,(intern (format "org-capture-ref-get-bibtex-%s" name)) ()
       (when-let ((link (org-capture-ref-get-bibtex-field :url)))
         (when (string-match ,link-match link)
           ,@body))))

  (defun mymy-org-capture-ref-get-date (string-or-dom)
    (let ((string (cond
                   ((stringp string-or-dom) string-or-dom)
                   ((stringp (car string-or-dom)) (mapconcat #'identity string-or-dom ""))
                   (t (dom-texts string-or-dom)))))
      (save-match-data
        (when (and string (string-match "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" string))
          (match-string 0 string)))))

  :config


  (mymy-org-capture-ref-get-bibtex- scott-young "scotthyoung\\.com"
    (mymy-org-capture-ref-set-bibtex-fields
      (:type "Online")
      (:howpublished "Scott H Young")
      (:doi org-capture-ref-placeholder-value)
      (:author (org-capture-ref-query-dom :meta "author"))
      (:title (org-capture-ref-query-dom :class "entry-title"))
      (:date (org-capture-ref-query-dom :meta "article:published_time" :apply #'mymy-org-capture-ref-get-date))
      (:year (org-capture-ref-query-dom :meta "article:published_time" :apply #'org-capture-ref-extract-year-from-string))))

  (add-to-list 'org-capture-ref-get-bibtex-functions #'org-capture-ref-get-bibtex-scott-young t)

  (mymy-org-capture-ref-get-bibtex- zettelkasten-de "zettelkasten\\.de"
    (mymy-org-capture-ref-set-bibtex-fields
      (:type "Online")
      (:doi org-capture-ref-placeholder-value)
      (:author (org-capture-ref-query-dom :meta "author"))
      (:title (org-capture-ref-query-dom :meta "og:title"))
      (:date (org-capture-ref-query-dom :meta "og:article:published_time" :apply #'mymy-org-capture-ref-get-date))
      (:year (org-capture-ref-query-dom :meta "og:article:published_time" :apply #'org-capture-ref-extract-year-from-string))))

  (add-to-list 'org-capture-ref-get-bibtex-functions #'org-capture-ref-get-bibtex-zettelkasten-de t)

  (setq org-capture-ref-capture-target (expand-file-name (concat mymy-org-roam-dir "2022-09-07-08-31-08-references.org"))
        org-capture-ref-capture-keys '("b" "B")
        org-capture-ref-capture-template `( :group "Browser link"
                                            :type entry
                                            :file ,org-capture-ref-capture-target
                                            :fetch-bibtex (lambda () (org-capture-ref-process-capture)) ; this must run first
                                            :extra "- [ ] Check bibtex entry for missing fields and incorrect metadata"
                                            :bibtex-string (lambda () (org-capture-ref-format-bibtex))
                                            :org-entry (lambda () (funcall org-capture-ref-headline-format-function))
                                            :template
                                            ("%{fetch-bibtex}* TODO %?%{space}%{org-entry}"
                                             ;; "%{extra}"
                                             "%{bibtex-string}"
                                             )
                                            :headline "References"
                                            :children (("Interactive link"
                                                        :keys ,(car org-capture-ref-capture-keys)
                                                        :clock-in t
                                                        :space " "
                                                        :clock-resume t)
                                                       ("Silent link"
                                                        :keys ,(cadr org-capture-ref-capture-keys)
                                                        :space ""
                                                        :immediate-finish t))))
  (let ((templates (doct org-capture-ref-capture-template)))
    (dolist (template templates)
      (asoc-put! org-capture-templates
                 (car template)
                 (cdr template)
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
  ((eww-mode . olivetti-mode)))

(use-package gnus
  :config
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
  (add-hook 'gnus-summary-mode-hook #'gnus-undo-mode)
  (setq gnus-asynchronous t))

(use-package deft
  :config
  (setq deft-extensions '("org"))
  (setq deft-directory (concat main-dropbox-dir "Notes"))
  (setq deft-recursive t))

(use-package anki-editor
  :disabled
  :config
  (setq anki-editor--ox-anki-html-backend
         (if anki-editor-use-math-jax
             (org-export-create-backend
              :parent 'html
              :transcoders '((latex-fragment . anki-editor--ox-latex-for-mathjax)
                             (latex-environment . anki-editor--ox-latex-for-mathjax)
                             (paragraph . strip-<p>-html)))
           (org-export-create-backend
            :parent 'html
            :transcoders '((latex-fragment . anki-editor--ox-latex)
                           (latex-environment . anki-editor--ox-latex)
                           (paragraph . strip-<p>-html))))))

(use-package org-drill :disabled)
(use-package quickrun)
(use-package olivetti
  :straight t
  :config
  (setq olivetti-body-width (+ 4 fill-column)))

;; I would normally take screenshots of org mode and source code and
;; send it through whatsapp
(use-package screenshot
  :straight (:type git :host github :repo "tecosaur/screenshot")
  )

(use-package nov
  :disabled
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (setq nov-text-width 60)
  :hook
  ((nov-mode . olivetti-mode)
   (nov-mode . (lambda () (face-remap-add-relative
                           'variable-pitch
                           :family "Liberation Serif"
                           :height 1.0)))
   (nov-mode . (lambda () (whitespace-mode -1)))))

(use-package treemacs
  :config
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window))
  )
(use-package projectile
  ;; TODO: Come here later
  :init
  (general-define-key
   :keymaps 'projectile-mode-map
   "C-c t" 'projectile-command-map)
  (setq-default projectile-project-search-path '("~/Projects/"))
  (setq projectile-completion-system 'auto)
  :hook
  (after-init . projectile-mode))

(use-package frames-only-mode
  ;; Set this to non-nil value if you want to have frame functionality
  :if (intern (or (getenv "EMACS_FRAMES") "nil"))
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
           "*poetry*"))
  (setq frames-only-mode-use-window-functions
         '( calendar report-emacs-bug checkdoc-show-diagnostics checkdoc org-compile-file
            embark-act dap-debug-run-task))
  :config
  ;; This is a temporary solution
  (unless (equal "DESKTOP-GTTJN7V" (system-name))
    (add-hook 'after-init-hook #'frames-only-mode))
  ;; (advice-add 'org-roam-dailies-capture-today :around 'frames-only-mode-advice-use-windows)
  :config
  (with-eval-after-load 'gnus
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
                                               (mml-preview 1.0 point)))))))

;; One moment I just thought "Dynamic tiling." And emacs can do this,
;; so maybe this will replace frames-only-mode
;; https://github.com/ajgrf/edwina
(use-package edwina
  ;; For now.
  :disabled
  :config
  (setq display-buffer-base-action '(display-buffer-below-selected))
  ;; (edwina-setup-dwm-keys)
  (edwina-mode 1)
  )

(use-package avy
  :init
  (require 'linum)
  :config
  (defun avy-goto-word-crt-line ()
    "Jump to a word start on the current line only."
    (interactive)
    (avy-with avy-goto-word-0
      (avy-goto-word-0 nil (line-beginning-position) (line-end-position))))

  :config
  (ryo-modal-keys ;;; Avy
    ("I" avy-goto-word-1)
    ("i" (("y" avy-goto-char)
          ("u" avy-goto-char-2)
          ("m" avy-goto-word-0)
          ("k" avy-goto-word-1)
          ("n" avy-goto-word-crt-line)
          ("i" avy-goto-line)
          ("/" (("m" avy-goto-word-0-below)
                ("k" avy-goto-word-1-below)))
          (";" (("m" avy-goto-word-0-above)
                ("k" avy-goto-word-1-above))))))
  :init
  (setq avy-case-fold-search nil)
  (setq avy-keys '(?n ?e ?i ?k ?y ?m ?u ?c ?r ?s ?t))
  :config
  (set-face-attribute 'avy-lead-face nil
                      :background "#818182" :foreground "#000000")
  (set-face-attribute 'avy-lead-face-0 nil
                      :background "#bdbca6" :foreground "#000000"))
(use-package ace-window
  :config
  (setq aw-keys '(?n ?e ?i ?o ?k ?m ?u ?y))
  (ace-window-display-mode)
  (ryo-modal-key "owf" #'ace-window))

(use-package windmove
  :config
  (windmove-default-keybindings)
  (setq windmove-wrap-around t))

(use-package yasnippet-snippets
  :defer 5)

(use-package yasnippet
  :bind (("TAB" . yas-expand))
  :config
  (add-to-list 'company-backends 'company-yasnippet t)
  ;; (add-to-list 'warning-suppress-types '(yasnippet backquote-change))

  (defun mymy-yasnippet-hook ()
    (yas-define-snippets
     'prog-mode
     '(;; "Key" "Template" "Name"
                                        ; git1: To give context the moment I do a commit
                                        ; git2: Make "g" snippet use comment depending on the mode
       ("g" "`(org-trim comment-start)` git: " "Git reminder")))
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
       (":h" ":hook" "Use package :hook")))
    (yas-define-snippets
     'python-mode
     '(;; "Key" "Template" "Name"
       ("pr" "print(${1:`(when (region-active-p) (yas-selected-text))`})$0" "print")))
    (yas-define-snippets
     'org-mode
     '(;; "Key" "Template" "Name"
       ("clj" "#+begin_src clojure\n\n#+end_src\n" "Clojure source block")
       ("<co" "#+STARTUP: content" "Content")
       ("<n" "#+STARTUP: nofold" "No fold")

       ;; Git: More easy to type
       (">el" "\\begin{${1:env}}\n\\end{${1:$(yas-field-value 1)}}" "Enviroment")
       (">a" "align" "Aling env")
       (">a*" "align*" "Align* env")
       (">e" "equation" "Equation env")
       (">e*" "equation*" "Equation* env")

       (">>frac" "\\frac{${1:a}}{${2:b}}" "Fraction")
       (">>div" "{${1:a} \\div ${2:b}}" "Division")
       (">>cdot" "{${1:a} \\cdot ${2:b}}" "Multiplication")
       (">>=" "{${1:a} = ${2:b}}" "Equality")
       (">>nck" "{${1:n} \\choose ${2:k}}" "Choose function")
       (">>int" "\\int_{${1:a}}^{${2:b}} ${3:f(x)} \\,dx" "Integrals")
       (">>sum" "\\sum_{${1:a}}^{${2:b}}" "Summation"))))

  (ryo-modal-keys
    ("gy" yas-visit-snippet-file)
    ("Gy" yas-insert-snippet))
  ;; :bind ((;; C-c y is reserved for yasnippets and, possibly, its
  ;;         ;; snippets shortcuts
  ;;         "C-c y y" . company-yasnippet))
  :hook
  ;; Doesn't work for some reason
  ;; (after-init . yas-global-mode)
  ;; (yas-global-mode-hook . mymy-yasnippet-hook)
  (prog-mode . yas-minor-mode)
  (text-mode . yas-minor-mode)
  ;; I'm really getting sure this things run
  (yas-minor-mode . mymy-yasnippet-hook))

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
  (lisp-mode . aggressive-indent-mode))

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
                           (projects . 5)))
  :bind (:map dashboard-mode-map
              ("TAB" . dashboard-next-section)
              ("<backtab>" . dashboard-previous-section))
  :config ;; For now i'm going to try something
  (setq inhibit-startup-screen t)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  ;; (setq initial-buffer-choice (concat org-roam-directory "Projects/20210715113548-projects.org"))
  (bookmark-location (car bookmark-alist))
  ;; TODO: Make own section of fast access files.
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
  ;; Not in current use
  :disabled
  :straight t
  ;; selected-<major-mode>-map for major mode specific keybindings
  ;; (setq selected-org-mode-map (make-sparse-keymap)) example org mode
  :commands selected-minor-mode
  :config
  (setq selected-minor-mode-override t)
  :bind (:map selected-keymap
              ("q" . selected-off)
              ("" . count-words-region)))
(use-package ace-mc
  ;; Not in current use
  :disabled
  :config
  (ryo-modal-keys
    ("i" (("a" ace-mc-add-multiple-cursors)
          ("t" ace-mc-add-single-cursor)))))

(use-package direx
  ;; Not in current use
  :disabled
  :config
  (ryo-modal-key "Okm" #'direx:jump-to-directory))

(use-package nyan-mode ;; Nyan, simple nyan
  :diminish nyan-mode
  :init
  (setq nyan-animate-nyancat t
        nyan-wavy-trail t)
  :hook
  (after-init . nyan-mode))

(use-package centaur-tabs
  :disabled
  :config
  (defun toggle-centaur-grouping ()
    (interactive)
    (if (eq centaur-tabs-buffer-groups-function 'centaur-tabs-projectile-buffer-groups)
        (centaur-tabs-group-buffer-groups)
      (centaur-tabs-group-by-projectile-project)))
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-change-fonts "Fantasque Sans Mono" 160)
  (setq centaur-tabs-style "bar"
         centaur-tabs-set-bar 'over
         centaur-tabs-set-icons t
         centaur-tabs-set-close-button nil
         centaur-tabs-cycle-scope 'tabs
         centaur-tabs-show-new-tab-button nil
         centaur-tabs-label-fixed-length 6)
  (setq uniquify-separator "/"
         uniquify-buffer-name-style 'forward)
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-get-groups)
  (ryo-modal-keys
    (">>" centaur-tabs-move-current-tab-to-right)
    ("<<" centaur-tabs-move-current-tab-to-left))
  :bind
  ("C-S-<iso-lefttab>" . centaur-tabs-backward)
  ("C-<tab>" . centaur-tabs-forward)
  ("C-z" . centaur-tabs-backward-group)
  ("C-S-z" . centaur-tabs-forward-group))

(use-package which-key ;; Useful to tell what is the next command that i can do
  :init
  (setq which-key-enable-extended-define-key t)
  (setq which-key-side-window-location 'bottom)
  (setq which-key-side-window-max-height 0.4)
  (setq which-key-side-window-max-width 0.66)
  :config
  (push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist)
  (which-key-mode)
  (which-key-add-key-based-replacements
    "ok" "Buffers, Files & and M-x"
    "oa" "Insert"
    "op" "Projectile"
    "ow" "Windows"
    "ot" "Bookmarks & Registers")
  (which-key-add-major-mode-key-based-replacements 'python-mode
    "sye" "Send statment"
    "syf" "Send function"
    "syc" "Send class"
    "sys" "Send top-statment"
    "syg" "Send group"
    "syw" "Send cell"
    "syr" "Send region"
    "syb" "Send buffer"
    "sy" "Eval code section"
    "s@" "HideShow"
    "rd" "Debugging"
    "rr" "Refractoring"))

(use-package expand-region
  :init
  (ryo-modal-key "\'" #'er/expand-region)
  :config
  (defhydra hydra-expand-region (:hint nil :color pink)
    "
_c_: Comment _d_: defun _w_: word _m_: method call _s_: symbol _a_: symbol with prefix
_qi_: Inside quotes _qo_: Outside quotes _pi_: Inside pairs _po_: Outside pairs _n_: Next accessor
_C-._ Deactivate mark _._: Quit.
"
    ("c" er/mark-comment)
    ("d" er/mark-defun)
    ("w" er/mark-word)
    ("m" er/mark-method-call)
    ("s" er/mark-symbol)
    ("a" er/mark-symbol-with-prefix)
    ("qi" er/mark-inside-quotes)
    ("qo" er/mark-outside-quotes)
    ("pi" er/mark-inside-pairs)
    ("po" er/mark-outside-pairs)
    ("n" er/mark-next-accessor) ;; skips over one period and mark next symbol
    ("C-." deactivate-mark :color blue)
    ("." nil :color blue)
    )
  (global-set-key (kbd "C-'") #'hydra-expand-region/body)
  (ryo-modal-keys
    ("\'" er/expand-region)
    ("On" (("c" er/mark-comment)
           ("d" er/mark-defun)
           ("w" er/mark-word)
           ("m" er/mark-method-call)
           ("s" er/mark-symbol)
           ("a" er/mark-symbol-with-prefix)
           ("q" (("i" er/mark-inside-quotes)
                 ("o" er/mark-outside-quotes)))
           ("p" (("i" er/mark-inside-pairs)
                 ("o" er/mark-outside-pairs)))
           ("n" er/mark-next-accessor) ;; skips over one period and mark next symbol
           )))
  :bind
  ("C-'" . er/expand-region))

(use-package yequake
  :disabled
  :after org-roam
  :config
  ;; Will change this to an advice later
  (el-patch-defun yequake-toggle (name)
    "Toggle the Yequake frame named NAME."
    (interactive (list (completing-read "Frame: " yequake-frames)))
    ;; This is from equake
    (el-patch-add
      (if-let ((graphic-frame (-first #'display-graphic-p (frame-list))))
          (select-frame graphic-frame t)))
    (if-let* ((frame (alist-get name yequake-frames nil nil #'string=)))
        (yequake--toggle-frame name frame)
      (user-error "No Yequake frame named: %s" name)))
  :config
  (setq yequake-frames
         '(("org-roam-dailies-capture-today"
            (buffer-fns . (yequake-org-roam-dailies-capture-today))
            (width . 0.75)
            (height . 0.5)
            (alpha . 0.8)
            (frame-parameters . ((undecorated . t)
                                 (sticky . t)
                                 (skip-taskbar . t)
                                 ;; (window-system . x)
                                 ))))))

(use-package terminal-here
  :config
  (global-set-key (kbd "C-<f5>") #'terminal-here-launch)
  (global-set-key (kbd "C-<f6>") #'terminal-here-project-launch)
  (setq terminal-here-linux-terminal-command '("kitty" "--single-instance"))
  (setq terminal-here-command-flag "--")
  (when (executable-find "poetry")
    (global-set-key (kbd "C-<f3>") (lambda () (interactive) (terminal-here-launch (list (executable-find "poetry") "shell")))))

  (defun mymy-set-xmonad-project-dir-here ()
    (interactive)
    (and (y-or-n-p "Want to change the xmonad directory?")
         (when (not (= 0 (shell-command (format (expand-file-name "~/Scripts/xmonadctl -a XMONAD_CHANGE_DIR %S") default-directory))))
           (error "Error setting the xmonad dir")))
    )
  (global-set-key (kbd "C-<f4>") #'mymy-set-xmonad-project-dir-here)
  )

;; let dir = default-directory
;; modifyProject (\p -> p { projectDirectory = dir })

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
    ("." nil :color blue)))

(use-package emacs
  :config
  (defun my-unfill-paragraph (&optional region)
    "Make multi-line paragraph into a single line of text.

REGION unfills the region.  See URL
`https://www.emacswiki.org/emacs/UnfillParagraph'"
    (interactive (progn (barf-if-buffer-read-only) '(t)))
    (let ((fill-column (point-max))
          ;; This would override `fill-column' if it's an integer.
          (emacs-lisp-docstring-fill-column t))
      (fill-paragraph nil region)))
  )

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setenv "PATH" (let ((current (getenv "PATH"))
                     (new (concat (getenv "HOME") "/.local/bin")))
                 (if current (concat new ":" current) new)))

(setq default-fill-column 74)
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
(setq use-dialog-box nil) ; Text-based options are better
;;; Seems this is not supported anymore, whatever it did
;; (setq bidi-display-reordering nil)
(setq bidi-inhibit-bpa t)
(setq-default bidi-paragraph-direction 'left-to-right)

;; Why this varible even exists?
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
(if (s-suffix? "laptop" (system-name))
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
(setq scroll-error-top-bottom t)
;; (setq-default
;;  prettify-symbols-alist
;;  '(("#+BEGIN_SRC" . "Λ")
;;    ("#+END_SRC" . "Λ")
;;    ("#+begin_src" . "Λ")
;;    ("#+end_src" . "Λ")
;;    ("lambda" . "λ")))
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

;; (setq exec-path (append exec-path '("~/.local/bin")))
(add-to-list 'exec-path "~/.local/bin")
(setq jit-lock-defer-time 0)
(setq fast-but-imprecise-scrolling t)
(setq auto-window-vscroll nil)
(setq savehist-additional-variables '(register-alist))
(setq split-height-threshold 4
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
    (defun xclip-paste-function ()
      (let* ((xclip-output
              (shell-command-to-string "xclip -o -selection clipboard"))
             (xclip-output (if (string= xclip-output "Error: target STRING not available\n")
                               "" xclip-output))
             )
        (unless (and (string= (car kill-ring) xclip-output) (string= xclip-output "Error: target STRING not available"))
          xclip-output)))
    (setq interprogram-cut-function 'xclip-cut-function)
    (setq interprogram-paste-function 'xclip-paste-function)))

;;                   Don't know where to put it, but it's still config
(with-eval-after-load 'linum
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



(add-hook 'after-init-hook
          (lambda ()
            (which-function-mode t)
            (global-undo-tree-mode)))
(add-hook 'prog-mode
          (lambda ()
            (auto-fill-mode t)))

(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

;; MyMy mode


;; git: Remove mymy-mode
;; Comment: General alredy provides a way of doing this
;; (define-minor-mode mymy-mode
;;   "Define all keys to have a preference to override others"
;;   :init-value nil
;;   :lighter " mymy"
;;   :keymap
;;   (let ((keymap (make-sparse-keymap)))
;;     (define-key keymap (kbd "S-SPC") 'ryo-modal-global-mode)
;;     (define-key keymap (kbd "M-t") 'ryo-modal-global-mode)
;;     keymap)
;;   :group 'mymy)
;; (define-globalized-minor-mode mymy-global-mode mymy-mode
;;   (lambda ()
;;     (if (not (minibufferp (current-buffer)))
;;         (mymy-mode t))))
;; (add-hook 'after-init-hook 'mymy-global-mode)

;; Load all functions
(require 'functions)
(ryo-modal-global-mode t)

;; Init.el ends here

