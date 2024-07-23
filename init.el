;; -*- lexical-binding: t; -*-
;;* Early
(when (version< emacs-version "27")
  (load (concat user-emacs-directory "early-init.el")))

;; (setq debug-on-error '(wrong-type-argument) debug-on-signal t)
;; (setq debug-on-error t)
;; https://github.com/bkaestner/.emacs.d/blob/37c75bfe3a199594ad89504d870e68f6f424764f/early-init.el
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(defun my-cleanup-gc ()
  "Clean up gc."
  (setq gc-cons-threshold 16777216)
  (garbage-collect))
;; After Emacs has completely started, reset the values to more sensible ones.
(add-hook 'emacs-startup-hook
          (lambda ()
            (progn
              (run-with-idle-timer 4 nil #'my-cleanup-gc)
              (setq gc-cons-threshold 16777216
                    gc-cons-percentage 0.1))))

;; (setq straight-check-for-modifications '(check-on-save find-when-checking))
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
;;* Straight bootstrap
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;; ;;* Straight config
;; (setq straight-use-package-by-default t)
;; (setq straight-host-usernames '((github . "cheerio-pixel")))
;;* vc-follows-symlinks
(setq vc-follow-symlinks t)
;; Install use-package support
(elpaca elpaca-use-package
        ;; Enable use-package :ensure support for Elpaca.
        (elpaca-use-package-mode))
;;* Use package installation
;; (straight-use-package '(org :type built-in))
;; (straight-use-package 'use-package)
;; (eval-when-compile (require 'use-package))

;;* Use package configuration

(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

;;* Require
(require 'cl-lib)

;;* Load path
(add-to-list 'load-path (concat user-emacs-directory "elisp/"))

;;* Important packages
;; This packages need to be loaded before all the others

(use-package general
  :ensure t
  :config
  (general-override-mode)
  )

(use-package exec-path-from-shell
  :ensure t
  :config
  (add-to-list 'exec-path-from-shell-variables "ANDROID_HOME")
  (add-to-list 'exec-path-from-shell-variables "ANDROID_SDK_ROOT")
  (when (or (memq window-system '(mac ns x))
            (daemonp))
    (exec-path-from-shell-initialize)))


;; Explicit patching of functions and variables.
(use-package el-patch
  :ensure (:wait t))

;; Bring a little bit of clojure and more
(use-package dash :ensure t :config (global-dash-fontify-mode))

(use-package s :ensure t)

(use-package hydra
  :after ryo-modal
  :ensure t
  ;;; Not in use
  :config
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
  (ryo-modal-key
   "q i" :hydra
   '(hydra-indent ()
                  "Indent Mode"
                  ("n" mymy/elpy-nav-move-line-or-region-down)
                  ("u" mymy/elpy-nav-move-line-or-region-up)
                  ("N" shift-left)
                  ("U" shift-right)
                  ("q" nil "cancel" :color blue)
                  ("." nil "cancel" :color blue)))
  )

;;* Alias
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'gsetq 'general-setq)
(defalias 'gsetq-local 'general-setq-local)
(defalias 'gsetq-default 'general-setq-default)

;;* Configs
;; (set-frame-parameter nil 'fullscreen 'fullboth) ; Fullscreen
;;* Modes
(global-hl-line-mode)
(setq-default show-trailing-whitespace t)
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

;; Let's test commenting this out
;; (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
;; (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;;* My variables
;; TODO: Set all of this in the defconst or defvar form
(setq dropbox-dir
      (pcase system-type
        ('windows-nt "c:/Users/frail/Dropbox/")
        ('gnu/linux "~/Dropbox (Maestral)/")))

(setq main-dropbox-dir (concat dropbox-dir "Creativè/"))

(setq mymy-org-roam-dir (concat main-dropbox-dir "Notes/"))

(defvar mymy-organization-system-directory (concat dropbox-dir "org/")
  "General purpose root directory of notes")

;; Check
(unless (file-exists-p mymy-organization-system-directory)
  (error "Cannot find '%s'. Directory doesn't exist " mymy-organization-system-directory))

(defvar mymy-organization-system-directory-attachments
  (concat mymy-organization-system-directory "attachments/")
  "Attachment directory")

(defvar mymy-bibliography-system-directory
  (concat mymy-organization-system-directory "bibliography_system/")
  "Diretory of bibliography references.")

(unless (file-exists-p mymy-bibliography-system-directory)
  (error "Cannot find '%s'. Directory doesn't exist " mymy-bibliography-system-directory))

(when (and (file-exists-p mymy-organization-system-directory)
           (not (file-exists-p mymy-organization-system-directory-attachments)))
  (make-directory mymy-organization-system-directory-attachments))

;;* Variables

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;;* Modeline

(use-package nyan-mode ;; Nyan, simple nyan
  ;; The fact that this works is incredible, but it has no more use
  :disabled
  :diminish nyan-mode
  :init
  (setq nyan-animate-nyancat t
        nyan-wavy-trail t)
  :hook
  (after-init . nyan-mode))

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



;;* Frame

(add-to-list 'default-frame-alist
             '(internal-border-width . 20))

(set-frame-parameter (selected-frame) 'internal-border-width 20)

(add-to-list 'default-frame-alist
             '(alpha-background . 99))


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

(defmacro section (description &rest body)
  "Progn but with a requisite of a string describing the body"
  (declare (indent 1))
  (or (stringp description) (error "Named-section should have a description"))
  `(progn ,@body))

;;* Functions

(defun mymy-path->qualified-name (path)
  (replace-regexp-in-string
   (regexp-quote "/")
   "."
   path))

(defun not-mymy-find-nearest-chsarp-project (&optional _dir)
  "Graciously stolen from https://github.com/sebasmonia/sharper/blob/master/sharper.el, all credit to the guy"
  (let ((start-from (or (buffer-file-name)
                        (when (eq major-mode
                                  'dired-mode)
                          default-directory))))
    (when start-from
      (locate-dominating-file
       start-from
       (lambda (directory)
         (when (file-directory-p directory)
           ;; Strangely enough, in Windows directory-files ignores a path
           ;; that is a file, but under Linux it fails. Adding a guard...
           (let ((files (directory-files directory t)))
             (cl-some (lambda (filename)
                        (let ((extension (file-name-extension filename)))
                          (member extension '("csproj" "fsproj"))))
                      files))))))))

(defun mymy-find-nearest-solution-file (&opitonal _dir)
  (let ((start-from (or (buffer-file-name)
                        (when (eq major-mode
                                  'dired-mode)
                          default-directory))))
    (when start-from
      (locate-dominating-file
       start-from
       (lambda (directory)
         (when (file-directory-p directory)
           ;; Strangely enough, in Windows directory-files ignores a path
           ;; that is a file, but under Linux it fails. Adding a guard...
           (let ((files (directory-files directory t)))
             (cl-some (lambda (filename)
                        (let ((extension (file-name-extension filename)))
                          (member extension '("sln"))))
                      files))))))))

(defvar mymy-process-to-json-script (concat
                                     user-emacs-directory
                                     "ps-aux-to-json.sh"
                                     ))

;; Linux
(defun mymy-processes-in-json ()
  "Get a list of process as list of plists
Schema: user, pid, cpu, mem, vsz, rss, tty, stat, start, time, command
"
  (json-parse-string
   (shell-command-to-string
    (concat "bash "
            mymy-process-to-json-script
            )
    )
   :object-type 'plist)
  )

(defun mymy-select-process (&optional filter-fn)
  "Select a running process.
FILTER-FN: Takes a plist of an object and returns true."
  (let* ((candidates-process (append (mymy-processes-in-json) nil))
         (candidates-process (if filter-fn (-filter filter-fn candidates-process)
                               candidates-process
                               ))
         (candidates (--map (cons (plist-get it :command) it) candidates-process))
         (selected (completing-read "Select process: " candidates nil t)))
    (cdr (assoc selected candidates))))



(comment
 ;; Doesn't work for some reason
 (defun xah-open-in-external-app (&optional Fname)
   "Open the current file or dired marked files in external app.
When called in emacs lisp, if Fname is given, open that.

URL `http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html'
Version: 2019-11-04 2023-04-05 2023-06-26"
   (interactive)
   (let (xfileList xdoIt)
     (setq xfileList
           (if Fname
               (list Fname)
             (if (eq major-mode 'dired-mode)
                 (dired-get-marked-files)
               (list buffer-file-name))))
     (setq xdoIt (if (<= (length xfileList) 10) t (y-or-n-p "Open more than 10 files? ")))
     (when xdoIt
       (cond
        ((eq system-type 'windows-nt)
         (let ((xoutBuf (get-buffer-create "*xah open in external app*"))
               (xcmdlist (list "PowerShell" "-Command" "Invoke-Item" "-LiteralPath")))
           (mapc
            (lambda (x)
              (message "%s" x)
              (apply 'start-process (append (list "xah open in external app" xoutBuf) xcmdlist (list (format "'%s'" (if (string-match "'" x) (replace-match "`'" t t x) x))) nil)))
            xfileList)
           ;; (switch-to-buffer-other-window xoutBuf)
           )
         ;; old code. calling shell. also have a bug if filename contain apostrophe
         ;; (mapc (lambda (xfpath) (shell-command (concat "PowerShell -Command \"Invoke-Item -LiteralPath\" " "'" (shell-quote-argument (expand-file-name xfpath)) "'"))) xfileList)
         )
        ((eq system-type 'darwin)
         (mapc (lambda (xfpath) (shell-command (concat "open " (shell-quote-argument xfpath)))) xfileList))
        ((eq system-type 'gnu/linux)
         (mapc (lambda (xfpath)
                 (call-process shell-file-name nil 0 nil
                               shell-command-switch
                               (format "%s %s"
                                       "xdg-open"
                                       (shell-quote-argument xfpath))))
               xfileList))
        ((eq system-type 'berkeley-unix)
         (mapc (lambda (xfpath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" xfpath))) xfileList)))))))

;; (defun mymy-hook-to-read-only-in-straigth-package ()
;;   "Open files under straight as read-only"
;;   (when (string-match-p (straight--dir) (buffer-file-name))
;;     (read-only-mode 1)))

;; (add-hook 'find-file-hook #'mymy-hook-to-read-only-in-straigth-package)

;; (defun append-date-to-file (file)
;;   "docstring"
;;   (interactive (buffer-file-name))
;;   (let (end-hook)
;;     (when (eq 'dired-mode major-mode)
;;       (setq file (dired-get-filename))
;;       (add-hook end-hook #'revert-buffer nil t))
;;     (unless (f-file-p file)
;;       (user-error "Current file is not valid"))
;;     (when (f-dir-p file)
;;       (user-error "Cannot rename directory with date"))
;;     (let ((filename (f-filename file))
;;           (path (concat (f-dirname file) "/"))
;;           ;; If you change one, obviously you have to change the other
;;           (file-date-regex "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}_")
;;           (date-string (format-time-string "%Y-%m-%d_"))
;;           new-filename)
;;       (if (string-match-p file-date-regex filename)
;;           (setq new-filename (concat path date-string filename))
;;         (setq new-filename (concat path (replace-regexp-in-string
;;                                          (concat file-date-regex "\\'") date-string filename))))
;;       (rename-file file new-filename)
;;       (run-hooks end-hookk))))

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

(defun mymy-lispy-different ()
  "Exchange point between parenthesis."
  (interactive)
  (cond ((looking-at "\\s\(")
         (forward-list))
        ((looking-back "\\s\)")
         (backward-list))
        ((looking-at "\\(\"\\|'\\)")
         (forward-sexp))
        ((looking-back "\\(\"\\|'\\)")
         ;; Has problem with actually going to the other side.
         (backward-sexp))
        ((derived-mode-p 'web-mode)
         (if (called-interactively-p)
             (call-interactively #'web-mode-navigate)
           (funcall #'web-mode-navigate)))
        (t nil)))

;;* Ryo modal

;; (use-package evil
;;   :config
;;   (evil-mode)
;;   (gsetq evil-undo-system 'undo-redo)
;;   (general-def 'normal emacs-lisp-mode-map
;;     "K" 'elisp-slime-nav-describe-elisp-thing-at-point)

;;   (general-override-mode 1)

;;   ;; (general-create-definer lift-def
;;   ;;   :states '(normal motion visual)
;;   ;;   :keymaps 'override
;;   ;;   :prefix "SPC"
;;   ;;   )

;;   (general-define-key
;;    :states '(normal motion visual)
;;    :keymaps 'override
;;    :prefix "SPC"

;;    "s" 'save-buffer
;;    "b" 'switch-to-last-buffer
;;    "fb" 'consult-buffer
;;    "ff" 'find-file
;;    "fx" 'reopen-killed-file
;;    "fX" 'reopen-killed-file-fancy
;;    "fl" #'consult-line
;;    "fg" #'consult-ripgrep
;;    )
;;   ;; SPC = Lift
;;   ;; f = prefix find
;;   ;; fb = Find buffer
;;   ;; ff = Find file
;;   ;; fx = Open last closed file
;;   ;; fX = Open history of closed files
;;   ;; fl = Consult line
;;   ;; fg = Consult ripgrep
;;   ;; fr = Find references of thing at point
;;   ;; r = prefix replace
;;   ;; rs = replace-string
;;   ;; rr = replace-regex
;;   )

;; (use-package evil-colllection
;;   :ensure (:type git :host github :repo "emacs-evil/evil-collection")
;;   :config
;;   (evil-collection-init '(dired consult))
;;   )

;; (use-package lispyville
;;   :init
;;   (general-add-hook '(emacs-lisp-mode-hook lisp-mode-hook) #'lispyville-mode)
;;   :config
;;   (lispyville-set-key-theme '(operators c-w additional)))

(use-package ryo-modal
  :ensure t
  :demand t
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

  (defun mymy-ryo-modal-update-default-after-theme-change-ad (&rest _)
    "If the theme is changed after loading ryo modal, then the default
cursor face is going to be configured as the previous theme cursor
face. This advice is to hook on the function `enable-theme'"
    (setq ryo-modal-default-cursor-color (face-attribute 'cursor :background)))

  (advice-add #'enable-theme :after #'mymy-ryo-modal-update-default-after-theme-change-ad)

  ;; Raise Ryo Modal map priority
  ;; Cannot do that, since ryo modal relies on making other modes to make its magic
  ;; (add-to-ordered-list 'emulation-mode-map-alists `((ryo-modal-mode . ,ryo-modal-mode-map)))
  :config
  (require 'ryo-modal-patch)

  (defvar ryo-modal-excluded-modes '()
    "A list of modes that the global mode should exclude.
By default the minibuffer is excluded.")

  (define-globalized-minor-mode ryo-modal-global-mode ryo-modal-mode
    (lambda ()
      (when (and (not (minibufferp (current-buffer)))
                 (not (member major-mode ryo-modal-excluded-modes)))
        (ryo-modal-mode t))))

  :config
  ;; r, s, R, S are reserved for major modes
  (with-eval-after-load 'iedit-mode
    (ryo-modal-key "f" #'iedit-mode))
  (defun mymy-downcase-char (arg)
    (interactive "p")
    (save-excursion
      (downcase-region (point) (progn (forward-char arg) (point)))))

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
          ("n" mymy-downcase-char)
          ("N" downcase-region)
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
    ("E" mymy-lispy-different)
    ;; ("c" forward-same-syntax)
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
                ("X" reopen-killed-file-fancy)) :name "Buffers, Files & and M-x")
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
    )
  ;; emacs-lisp
  (ryo-modal-major-mode-keys
    'emacs-lisp-mode
    ("s" (("s" eval-buffer)
          ("r" eval-region)
          ("f" eval-defun)))))

(use-package yasnippet
  :after ryo-modal
  :ensure t
  ;; :bind (("TAB" . yas-expand))
  :config
  ;; (add-to-list 'company-backends 'company-yasnippet t)
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
       (":s" ":ensure " "Use package :straigth")
       (":ss" ":ensure (:host github :type git :repo \"${1:repo}\" :files (${2:files}))" ":ensure repo")
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

  (add-hook 'prog-mode-hook #'yas-minor-mode)

  ;; (add-hook 'prog-mode-hook #'yas-minor-mode)
  ;; (add-hook 'text-mode-hook #'yas-minor-mode)
  ;; (add-hook 'yas-minor-mode-hook #'mymy-yasnippet-hook)
  (ryo-modal-keys
    ("gy" yas-visit-snippet-file)
    ("Gy" yas-insert-snippet))
  ;; :bind ((;; C-c y is reserved for yasnippets and, possibly, its
  ;;         ;; snippets shortcuts
  ;;         "C-c y y" . company-yasnippet))
  ;; :hook
  ;; ;; Doesn't work for some reason
  ;; ;; (after-init . yas-global-mode)
  ;; ;; (yas-global-mode-hook . mymy-yasnippet-hook)
  ;; (prog-mode . yas-minor-mode)
  ;; (text-mode . yas-minor-mode)
  ;; ;; I'm really getting sure this things run
  ;; (yas-minor-mode . mymy-yasnippet-hook)
  )

;; * All previous packages are somehow used later on and do not depend on
;; each other at the package dependency level.
(elpaca-wait)


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
  ;; "C-b"
  ;; "C-f"
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
 ;; "C-r" 'backward-char
 ;; "C-s" 'forward-char
 ;; "M-r" 'backward-word
 ;; "M-s" 'forward-to-word
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
 "C-a" 'smarter-move-beginning-of-line
 "M-m" 'new-line-dwim
 "M-e" 'hippie-expand
 "M-n" 'dabbrev-expand
 "C-c s u" 'straight-use-package
 "C-c s g" 'straight-get-recipe
 "C-;" 'iedit-mode
 "C-M-;" 'iedit-mode
 "C-x C-y" 'pp-macroexpand-last-sexp
 "M-<" #'xref-go-back
 "M->" #'xref-go-forward
 )

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
;; I grew up
;; (general-define-key
;;  :keymaps 'key-translation-map
;;  "C-p" "C-u"
;;  "C-u" "C-p")
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
 "C-t" 'ryo-modal-global-mode
 )
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

;;* Set the font
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fantasque Sans Mono" :foundry "outline" :slant normal :weight normal :height 130 :width normal))))
 '(olivetti-fringe ((t (:foreground "#353535" :background "#353535")))))

;; This assumes you've installed the package via MELPA.
(use-package ligature
  :ensure t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes

  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;;* Emacs theme


(use-package idea-darkula-theme
  :ensure t
  :init
  (setq custom--inhibit-theme-enable nil)
  ;; (setq custom--inhibit-theme-enable 'apply-only-user)
  :config
  (push (substitute-in-file-name "~/.emacs.d/idea-darkula-theme/") custom-theme-load-path)
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

(section "Text mode indentation"
  ;; Put a more comfortable fill-column
  (setq-default fill-column 75)

  ;; Run olivetti, adaptative-wrap and visual-line mode to
  (add-hook 'text-mode #'visual-line-mode)

  (use-package adaptive-wrap
    :hook (text-mode . adaptive-wrap-prefix-mode)
    :init
    (setq adaptive-wrap-extra-indent 0)
    )

  (use-package olivetti
    ;; :disabled
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
  )

(use-package persid
  :ensure (:type git :host github :repo "rougier/persid"))

;;** Vertico
(use-package vertico-posframe
  :disabled t
  :after (vertico)
  :ensure t)

(use-package vertico
  :ensure t
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
  (defun vertico--filter-completions (&rest args)
    "Compute all completions for ARGS with lazy highlighting."
    (dlet ((completion-lazy-hilit t) (completion-lazy-hilit-fn nil))
      (if (eval-when-compile (>= emacs-major-version 30))
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
	  (cons (apply #'completion-all-completions args) completion-lazy-hilit-fn)))))

  )

(with-eval-after-load 'ryo-modal
  (section "Bookmarked files"
    (defvar mymy-bookmarked-files '()
      "Files saved for later search."
      )

    (defvar mymy-bookmarked-files-save-file
      (expand-file-name (concat user-emacs-directory
                                "bookmarked-files.eld")))

    (defun mymy-bookmarked-files-init ()
      (if (file-exists-p mymy-bookmarked-files-save-file)
          (->> mymy-bookmarked-files-save-file
               (mymy-bookmarked-files-load)
               (mymy-bookmarked-files-clean)
               (mymy-bookmarked-files-save mymy-bookmarked-files-save-file)
               (setq mymy-bookmarked-files)
               )
        (mymy-bookmarked-files-save
         mymy-bookmarked-files-save-file
         mymy-bookmarked-files)))

    (add-hook 'after-init-hook #'mymy-bookmarked-files-init)

    (defun mymy-bookmarked-files-clean (files)
      "Function that cleans the files in bookmarked files"
      (-filter (-compose #'file-exists-p #'expand-file-name) files))

    (defun mymy-bookmarked-files-save (save-file files)
      (cl-assert (not (file-directory-p save-file)))
      (with-temp-file save-file
        (erase-buffer)
        (insert (format "%S" mymy-bookmarked-files)))
      mymy-bookmarked-files)

    (defun mymy-bookmarked-files-load (save-file)
      (cl-assert (and (file-exists-p save-file)
                      (not (file-directory-p save-file))))
      (with-temp-buffer
        (insert-file-contents save-file)
        (-> (buffer-substring-no-properties (point-min)
                                            (point-max))
            (read-from-string)
            (car))))

    (defun mymy-bookmarked-files-add (file)
      (interactive (list (car (find-file-read-args "Find file: "
                                                   (confirm-nonexistent-file-or-buffer)))))
      (add-to-list 'mymy-bookmarked-files file)
      (mymy-bookmarked-files-save mymy-bookmarked-files-save-file
                                  mymy-bookmarked-files))

    (defun mymy-bookmarked-files-delete (file)
      (interactive (list (completing-read "Files: "
                                          mymy-bookmarked-files
                                          nil
                                          t)))
      (when file
        (setq mymy-bookmarked-files (remove file mymy-bookmarked-files))
        (mymy-bookmarked-files-save
         mymy-bookmarked-files-save-file
         mymy-bookmarked-files)))

    (ryo-modal-key "otf" #'mymy-bookmarked-files-add)

    (ryo-modal-key "otr" #'mymy-bookmarked-files-delete)

    (with-eval-after-load 'consult
      (defvar consult--source-file-bookmarks
        `( :name     "File Bookmarks"
           :category file
           :narrow   ?i
           :face     consult-file
           :history  file-name-history
           :state    ,#'consult--file-state
           :new      ,#'consult--file-action
           :items    ,(lambda ()
                        mymy-bookmarked-files)))

      (add-to-list 'consult-buffer-sources 'consult--source-file-bookmarks 'append)
      )))

(use-package consult
  :ensure t
  :config
  (setq consult-fontify-max-size 1024)
  :config
  (defun consult-grep-one-file ()
    "Call `consult-grep' for the current buffer (a single file)."
    (interactive)
    (consult-grep (list (shell-quote-argument buffer-file-name))))

  (defun consult-ripgrep-single-file ()
    "Call `consult-ripgrep' for the current buffer (a single file)."
    (interactive)
    (let ((consult-project-function (lambda (x) nil)))
      (consult-ripgrep (list (shell-quote-argument buffer-file-name)))))

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

  ;; Yeah, super redundant, that is the point.
  (defun mymy-consult-grep-change-depending-on-arg (arg &optional dir initial)
    "Change consult grep depending on arg"
    (interactive "P")
    (cond
     ((equal arg '(4))
      (consult-grep dir initial))
     (t
      (consult-ripgrep dir initial))))

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
        xref-show-definitions-function #'consult-xref)
  :config
  (ryo-modal-keys
    ;; ("F" mymy-consult-line-using-thing-at-point)
    ;; ("F" mymy-consult-line-using-region)
    ;; ("F" mymy-consult-line-cycle)
    ("f" consult-line)
    ;; Remember that ! SPC negates the focus
    ;; ("F" consult-keep-lines)
    ("F" consult-line)
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
           ("f" mymy-consult-grep-change-depending-on-arg)
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
  :ensure (:type git :host github :repo "mohkale/consult-bibtex")
  :config
  (require 'consult-bibtex-embark)
  (ryo-modal-key "Seb" 'consult-bibtex)
  (setq consult-bibtex-default-action #'consult-bibtex-edit-notes)
  (define-key consult-bibtex-embark-map "RET" #'consult-bibtex)
  (with-eval-after-load 'embark
    (add-to-list 'embark-become-keymaps 'consult-bibtex-embark-map)))

(use-package marginalia
  :ensure t
  :config
  ;; Until I find the way.
  ;;; I don't remember why I said the previous thing
  (marginalia-mode)
  )

(use-package embark
  :ensure t
  ;; Unnecessary? Maybe, but this thing wasn't loading symlinking every .el file so I had to put it myself.
  ;; :ensure (:files ("*.el"))
  :config
  (general-define-key
   "C-," 'embark-act
   "M-," 'embark-dwim
   "C-c i" 'embark-act
   )

  (setq display-buffer-alist
        '())

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
  :after (embark consult)
  :ensure t
  ;; :no-require t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  )


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
  (setq orderless-matching-styles '( orderless-literal orderless-regexp ;; orderless-flex
                                     
                                     ))
  (setq completion-category-overrides '((file (styles basic partial-completion substring))))
  )

(use-package fussy
  :disabled
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

  (with-eval-after-load 'consult
    (defun mymy-dont-messup-consult-line-ad (orig-f &rest args)
      "Use fussy for everything, except this."
      (let ((orderless-matching-styles '(orderless-literal
                                         orderless-regexp
                                         orderless-prefixes))
            (completion-styles '(basic orderless)))
        (apply orig-f args)))
    (general-add-advice #'consult-line :around #'mymy-dont-messup-consult-line-ad))

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
  :ensure t
  :ensure helm-swoop helm-projectile
  :diminish helm-mode
  :init
  (setq helm-split-window-default-side 'rigth)
  (setq helm-bookmark-show-location t)
  (setq helm-buffers-fuzzy-matching t)
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
    (write-region "" nil ispell-personal-dictionary nil 0)))

;;** Niceties

(use-package electric-operator
  :ensure t)
(use-package highlight-indentation
  :ensure t
  :config (highlight-indentation-mode 1))
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package git-gutter
  :disabled
  :config
  (gsetq git-gutter:update-interval 0.05)
  (gsetq git-gutter:modified-sign "*")
  (gsetq git-gutter:added-sign "+")
  (gsetq git-gutter:deleted-sign "-")

  ;; (set-face-foreground 'git-gutter:modified "yellow") ;; background color
  ;; (set-face-foreground 'git-gutter:added "green")
  ;; (set-face-foreground 'git-gutter:deleted "red")

  (defun mymy-git-gutter-hook ()
    (git-gutter-mode))

  (defun my/git-commit-reminder ()
    (let ((change-count (git-gutter:statistic))
          (timeout 300)) ;; 5 minutes in seconds
      (when (or (> (+ (car change-count)
                      (cdr change-count))
                   100) ;; Change this threshold as needed
                (and (> timeout 0)
                     (not (sit-for timeout))))
        (alert "Don't forget to commit your changes!" :severity 'moderate))))

  (add-hook 'git-gutter:update-hooks 'my/git-commit-reminder)

  (add-hook 'prog-mode-hook #'mymy-git-gutter-hook)
  (add-hook 'text-mode-hook #'mymy-git-gutter-hook))

(use-package git-gutter-fringe
  :disabled
  :after (git-gutter)
  :demand fringe-helper
  :config
  (setq fringes-outside-margins t)
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [224] nil nil '(center repeated))
  )

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

(use-package wgrep
  :ensure t)

(use-package ag
  :ensure t)
;; sp-pair is not suitiable when you have strict-mode activate
(use-package wrap-region
  ;; Shouldn't be using this
  :disabled
  :ensure (:type git :host github :repo "cheerio-pixel/wrap-region.el")
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
  :ensure t
  :config
  (add-hook 'lisp-data-mode-hook
            #'aggressive-indent-mode)
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

(use-package goto-chg
  :ensure t
  :config
  (ryo-modal-keys
    ("g;" goto-last-change)
    ("G:" goto-last-change-reverse)))

;;** Web programming
(defun mymy-vue-hook ()
  (setq-local web-mode-script-padding 0)
  (setq-local prettier-js-args '("--parser vue --tab-width 4"))
  ;; (prettier-js-mode)
  )

(defun mymy-html-web-hook  ()
  (when (and (buffer-file-name)
             (string-match-p "\\.html\\'" (buffer-file-name)))
    (lsp)
    )
  )

(use-package emmet-mode
  :ensure t)

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

  (add-hook 'web-mode-hook #'emmet-mode)
  (add-hook 'web-mode-hook #'emmet-mode)
  (add-hook 'vue-web-mode-hook #'mymy-vue-hook)
  (add-hook 'php-web-mode-hook #'lsp)
  )

(use-package lsp-razor
  :after (lsp-mode web-mode)
  :ensure nil
  :load-path "lsp-razor.el"
  :init
  (add-to-list 'lsp-language-id-configuration '(razor-web-mode . "aspnetcorerazor"))

  (defun mymy-lsp-razor-hook ()
    (setq-local comment-start "@*")
    (setq-local comment-end "*@")
    (lsp)
    )

  (add-to-list 'treesit-extra-load-path "~/.cache/tree-sitter/lib/")
  (add-to-list 'treesit-load-name-override-list '(razor "razor"))

  (require 'lsp-razor)

  :hook
  (razor-web-mode . mymy-lsp-razor-hook)
  )

(use-package vue-mode
  :disabled)

(use-package vue-ts-mode
  ;; Stopped working with
  ;; Error during redisplay: (jit-lock-function 1) signaled (wrong-type-argument treesit-node-p nil)
  ;; For some reason this stops if it doesn't have the <source> tag
  :disabled
  :ensure (:type git :host github :repo "8uff3r/vue-ts-mode")
  :hook ((vue-mode-ts . eglot-ensure))
  :init
  (add-hook 'vue-mode-ts-hook #'mymy-vue-hook))

(use-package prettier-js
  :ensure t
  ;; Check prettier's comamnd-line help options to see how to configure
  ;; (setq-local prettier-js-args '("--parser vue --tab-width 4"))
  :preface
  (unless (executable-find "prettier")
    (error "prettier is not installed. Consider installing with `pacman -S prettier`")))
;; Not using for now
;; (use-package impatient-mode)

;; (use-package centered-cursor-mode
;;   :ensure (:type git :host github :repo "andre-r/centered-cursor-mode.el" :branch "dev")
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
  :ensure t
  :init
  (defun mymy/smartparens-hook ()
    (smartparens-global-mode)
    (show-smartparens-global-mode))
  (setq mymy-lisp-modes '(emacs-lisp-mode clojure-mode cider-mode slime-mode lisp-mode))
  :config (sp-local-pair mymy-lisp-modes "'" "'" :actions nil)
  :hook ((after-init . mymy/smartparens-hook)
         (prog-mode . smartparens-strict-mode)
         (haskell-mode . (lambda () (require 'smartparens-haskell)))
         ))

(when (version< emacs-version "29")
  (use-package explain-pause-mode
    :ensure (explain-pause-mode :type git :host github :repo "lastquestion/explain-pause-mode")
    :config
    (explain-pause-mode)))

;;** Golden ration
;; Desc: Zoom emacs windows with the golden ratio
(use-package golden-ratio
  :disabled
  :config (golden-ratio-mode t)
  (add-to-list 'golden-ratio-extra-commands 'ace-window)
  (ryo-modal-key "gr" #'golden-ratio-mode))

;;** Spaceline

(use-package spaceline
  :disabled
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
  (gsetq powerline-default-separator 'wave)
  (spaceline-compile)
  :config
  (el-patch-defcustom powerline-default-separator 'arrow
    "The separator to use for the default theme.

Valid Values: alternate, arrow, arrow-fade, bar, box, brace,
butt, chamfer, contour, curve, rounded, roundstub, wave, zigzag,
slant, utf-8."
    (el-patch-add
      :set (lambda (var val) (set var val) (spaceline-compile)))
    :group 'powerline
    :type '(choice (const alternate)
                   (const arrow)
                   (const arrow-fade)
                   (const bar)
                   (const box)
                   (const brace)
                   (const butt)
                   (const chamfer)
                   (const contour)
                   (const curve)
                   (const rounded)
                   (const roundstub)
                   (const slant)
                   (const wave)
                   (const zigzag)
                   (const utf-8)
                   (const nil))))

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
  :ensure nil
  :load-path (concat user-emacs-directory "elisp/rainbow-numbers-mode.el"))

(use-package alert
  :ensure t
  :init
  (setq alert-default-style 'libnotify)
  )

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
  :after (magit)
  :config (magit-todos-mode 1))



;;** Corfu


(use-package corfu
  ;; Explictly tell that we want all the files in extensions (not
  ;; necessary, but don't want be manually dealing with this) since this is
  ;; only building corfu.el
  ;; :ensure (corfu :files ("*.el" "extensions/*.el"))
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
  :after (corfu)
  :config
  (general-define-key
   :keymap 'org-mode-map
   "C-M-k" 'cape-file))

(use-package nerd-icons-corfu
  :ensure (:host github :type git :repo "LuigiPiucco/nerd-icons-corfu")
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

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
  :after (frames-only-mode)
  :if (not frames-only-mode)
  :config
  (general-define-key
   :keymaps 'company-active-map
   "C-c h" 'company-quickhelp-manual-begin)
  (unless frames-only-mode
    (company-quickhelp-mode))
  )

;; (use-package ses-mode
;;   :no-require
;;   :ensure nil
;;   :config
;;   (setq ses-after-entry-functions '(next-line))
;;   :hook
;;   (ses-mode . rainbow-numbers-mode))
;;** Lisp things
(use-package lispy
  :ensure t
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
  :disabled
  :ensure clj-refactor pkg-info
  ;; :config
  ;; (cljr-add-keybindings-with-prefix "C-c m")
  :hook
  (cider-mode . clj-refactor-mode))

(use-package flycheck-clj-kondo
  :ensure t)

(use-package cider
  :ensure t
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
  :ensure t
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

;; (use-package edbi)

(use-package ejc-sql
  :ensure t
  :config
  (setq clomacs-httpd-default-port 8090) ; Use a port other than 8080.
  (setq ejc-result-table-impl 'orgtbl-mode)

  (add-hook 'ejc-sql-minor-mode-hook
            (lambda ()
              (ejc-eldoc-setup)))

  (defun ejc-buffer-kill-hook ()
    "Hook function to handle buffer deletion."
    (when (ejc-buffer-connected-p)
      (ejc-quit-connection)
      )
    )

  ;;; Should run before kill-buffer
  (add-hook 'kill-buffer-hook 'ejc-buffer-kill-hook)

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
          ("mssql_test"
           (:classpath .
                       ["/home/cheerio-pixel/.m2/repository/com/microsoft/sqlserver/mssql-jdbc/6.2.2.jre8/mssql-jdbc-6.2.2.jre8.jar"])
           (:password . "TestTest123")
           (:user . "MSSQLTest")
           (:port . "1433")
           (:host . "localhost")
           (:dbname . "DBAvanzado")
           (:dbtype . "sqlserver"))
          ("mssql_admin"
           (:classpath .
                       ["/home/cheerio-pixel/.m2/repository/com/microsoft/sqlserver/mssql-jdbc/6.2.2.jre8/mssql-jdbc-6.2.2.jre8.jar"])
           (:password . "1Kind prince")
           (:user . "sa")
           (:port . "1433")
           (:host . "localhost")
           (:dbname . "master")
           (:dbtype . "sqlserver"))
          ("LabProg1"
           (:classname . "com.mysql.jdbc.Driver")
           (:classpath .
                       ["/home/cheerio-pixel/.m2/repository/mysql/mysql-connector-java/5.1.44/mysql-connector-java-5.1.44.jar"])
           (:password . "testtest")
           (:user . "MySQL test")
           (:port . "3306")
           (:host . "localhost")
           (:dbname . "LabProg1")
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

(use-package lua-mode
  :ensure t
  ;; TODO: Delete when updating to emacs 30, since there is a lua-ts-mode
  :hook (lua-mode . lsp)
  )

(use-package dart-mode
  :ensure t
  :init
  (with-eval-after-load "projectile"
    (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
    (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

  :hook (dart-mode . lsp))

(use-package lsp-dart
  :ensure t
  :after (lsp-mode)
  :init
  ;; In arch linux, as of 05/05/2024, when installing the flutter-bin
  ;; package, the sdk is in /opt/flutter, but the excutables are in
  ;; /usr/bin or /bin, which causes some problems for lsp-dart when it
  ;; tries to find the sdk.
  (setq lsp-dart-sdk-dir "/opt/flutter")

  (el-patch-defun lsp-dart-dap--populate-flutter-start-file-args (conf)
    "Populate CONF with the required arguments for Flutter debug."
    (let ((pre-conf (-> conf
                        lsp-dart-dap--base-debugger-args
                        (dap--put-if-absent :type "flutter")
                        (dap--put-if-absent :flutterMode "debug")
                        (dap--put-if-absent :program (or (lsp-dart-get-project-entrypoint)
                                                         (buffer-file-name))))))
      (lambda (start-debugging-callback)
        (lsp-dart-dap--flutter-get-or-start-device
         ((el-patch-swap -lambda lambda)
          (el-patch-swap ((&hash "id" device-id "name" device-name))
                         (device)
                         )
          (el-patch-wrap 2 0
            (let ((device-id (lsp-get device :id))
                  (device-name (lsp-get device :name)))
              (funcall start-debugging-callback
                       (-> pre-conf
                           (dap--put-if-absent :deviceId device-id)
                           (dap--put-if-absent :deviceName device-name)
                           (dap--put-if-absent :dap-server-path (if (lsp-dart-dap-use-sdk-debugger-p)
                                                                    (append (lsp-dart-flutter-command) (list "debug_adapter" "-d" device-id))
                                                                  lsp-dart-dap-flutter-debugger-program))
                           (dap--put-if-absent :flutterPlatform "default")
                           (dap--put-if-absent :toolArgs `("-d" ,device-id))
                           (dap--put-if-absent :name (concat "Flutter (" device-name ")")))))))))))
  )

(use-package hover
  ;; I don't know why this doesn't work (The executable)
  :disabled
  :after (dart-mode lsp-dart)
  :bind (:map hover-minor-mode-map
              ("C-M-z" . #'hover-run-or-hot-reload)
              ("C-M-x" . #'hover-run-or-hot-restart)
              ("C-M-p" . #'hover-take-screenshot))
  :init
  (setq hover-flutter-sdk-path nil
        hover-command-path nil)

  (setq hover-hot-reload-on-save t
        hover-screenshot-path (concat (getenv "HOME") "/Pictures")
        hover-screenshot-prefix "my-prefix-"
        hover-observatory-uri "http://my-custom-host:50300"
        hover-clear-buffer-on-hot-restart t)
  (hover-minor-mode 1))


(use-package dart-ts-mode
  ;; For some reason the syntax highlighting is incomplete.
  :disabled
  :ensure (:host github :type git :repo "50ways2sayhard/dart-ts-mode")
  :config
  ;; For all-the-icons users:
  (with-eval-after-load 'all-the-icons
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(dart-ts-mode all-the-icons-fileicon "dart" :height 1.0 :face all-the-icons-blue)))

  (comment
   ;; For nerd-icons users:
   (add-to-list 'nerd-icons-mode-icon-alist '(dart-ts-mode nerd-icons-devicon "nf-dev-dart" :face nerd-icons-blue))
   )
  )

(use-package tree-sitter
  :disabled
  :ensure tree-sitter-langs
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

(use-package sharper
  :after (transient)
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
   "mvc"
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
  :no-require
  :ensure nil
  :config
  (defun mymy-csharp-mode-hook ()
    (setq-local flycheck-navigation-minimum-level 'error)
    )
  (add-hook 'csharp-ts-mode-hook #'mymy-csharp-mode-hook)

  (comment
   (let  ((diagnostic (lsp-diagnostics t)))
     (gethash (buffer-file-name) diagnostic)
     )
   (let  ((diagnostic (lsp-diagnostics t)))
     (maphash (lambda (key value) (message "%S" value)) diagnostic)
     )
   )
  ;; (add-hook 'csharp-ts-mode-hook #'aggressive-indent-mode)
  )

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

;; Maybe init lsp
;; (require 'init-lsp)

(use-package lsp-bridge
  ;; It's good but also kind of slow
  :disabled
  :ensure '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
                       :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                       :build (:not compile))
  :init
  ;; RUN pip3 install epc orjson sexpdata six setuptools paramiko rapidfuzz
  ;; RUN pacman -S ruff ruff-lsp
  (global-lsp-bridge-mode)

  (defun eat/lsp-bridge-mode-setup ()
    (interactive)
    (ignore-errors
      (company-mode -1)
      (corfu-mode -1)))


  (add-hook 'lsp-bridge-mode-hook #'eat/lsp-bridge-mode-setup)
  )

(use-package lsp-mode
  :ensure (:host github :type git :repo "emacs-lsp/lsp-mode")
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-completion-provider :none)
  ;; (setq lsp-enable-snippet nil)
  (setq lsp-enable-snippet t)
  ;; (setq lsp-log-io nil)
  ;; (setq lsp-log-io t)
  ;; (setq lsp-log-io-allowlist-methods '("textDocument/completion" "razor/completion"))
  ;; (setq lsp-log-io-allowlist-methods '("completionItem/resolve" "window/logMessage"))
  ;; (setq lsp-log-io-allowlist-methods '("completionItem/resolve"))
  ;; (setq lsp-log-io-allowlist-methods '("window/logMessage"))
  ;; (setq lsp-log-io-allowlist-methods '())

  (setq lsp-csharp-server-path (expand-file-name "~/tmp/omnisharp-roslyn/bin/Release/OmniSharp.Stdio.Driver/net6.0/OmniSharp"))
  (setq lsp-csharp-server-path nil)

  (section "CSharp lsp"
    ;; There is a problem with dotnet, it uses the incorrect RID in arch
    ;; linux which causes the server to fail at finding asp net core.

    ;; https://stackoverflow.com/questions/77437802/missing-package-microsoft-netcore-app-host-arch-x64-dotnet-restore-error
    ;; Doesn't do sh*t but it's a good reminder
    (setenv "DOTNET_RUNTIME_ID" "linux-x64")
    ;; (setenv "DOTNET_RUNTIME_ID" "arch-x64")
    )

  (setq lsp-disabled-clients
        '(semgrep-ls emmet-ls))

  (section "Angular lsp"
    (setq lsp-clients-angular-language-server-command
          '("node"
            "/usr/lib/node_modules/@angular/language-server"
            "--ngProbeLocations"
            "/usr/lib/node_modules"
            "--tsProbeLocations"
            "/usr/lib/node_modules"
            "--stdio")))

  (section "SQL lsp"
    ;; Symlinked the sqls to /usr/bin from ~/go/bin/sqls
    ;; https://emacs-lsp.github.io/lsp-mode/page/lsp-sqls/
    (setq lsp-sqls-connections
          ;; TODO: Configure mysql
          '(;; ((driver . "mysql") (dataSourceName . "yyoncho:local@tcp(localhost:3306)/foo"))
            ((driver . "mssql") (dataSourceName . "Server=localhost;Database=DBAvanzado;User Id=MSSQLTest;Password=TestTest123;")))))

  :config
  (define-key lsp-mode-map (kbd "M-?") #'lsp-find-references)
  (define-key lsp-mode-map (kbd "M-/") #'lsp-find-implementation)
  (define-key lsp-mode-map (kbd "M-.") #'lsp-find-definition)
  (define-key lsp-mode-map (kbd "C-;") #'lsp-iedit-highlights)
  (define-key lsp-mode-map (kbd "C-M-;") #'lsp-iedit-highlights)
  ;; Ensure lsp-volar--activate-p is defined before patching
  (require 'lsp-volar)
  ;; Also takes over in non vue projects, which is kind of annoying.
  (el-patch-defun lsp-volar--activate-p (filename &optional _)
    "Check if the volar-language-server should be enabled base on FILENAME."
    (if lsp-volar-take-over-mode
        (or (or
             (and (lsp-workspace-root) (lsp-volar--vue-project-p (lsp-workspace-root)))
             (and (lsp-workspace-root) lsp-volar-activate-file (f-file-p (f-join (lsp-workspace-root) lsp-volar-activate-file))))
            (or (el-patch-remove
                  ;; Why would I want a volar server in a non vue project?
                  (or (string-match-p "\\.mjs\\|\\.[jt]sx?\\'" filename)
                      (and (derived-mode-p 'js-mode 'typescript-mode 'typescript-ts-mode)
                           (not (derived-mode-p 'json-mode)))))
                (string= (file-name-extension filename) "vue")))
      (string= (file-name-extension filename) "vue")))

  (setq lsp-auto-execute-action nil)
  ;; (el-patch-defun lsp--select-action (actions)
  ;;   "Select an action to execute from ACTIONS."
  ;;   (cond
  ;;    ((seq-empty-p actions) (signal 'lsp-no-code-actions nil))
  ;;    ;; There is no way to know what action it may execute, so you know.
  ;;    (el-patch-remove
  ;;      ((and (eq (seq-length actions) 1) lsp-auto-execute-action)
  ;;       (lsp-seq-first actions)))
  ;;    (t (let ((completion-ignore-case t))
  ;;         (lsp--completing-read "Select code action: "
  ;;                               (seq-into actions 'list)
  ;;                               (-compose (lsp--create-unique-string-fn)
  ;;                                         #'lsp:code-action-title)
  ;;                               nil t)))))

  ;; Around that line
  ;; https://github.com/TheBB/dotemacs/blob/master/lisp/bb-defs.el#L505-L506
  (defun bb-lsp-set-priority (server priority)
    (setf (lsp--client-priority (gethash server lsp-clients)) priority))

  (defun bb-lsp-priority (server)
    (lsp--client-priority (gethash server lsp-clients)))

  (section "Extension for CSharp"

    "Decompilation"

    ;; Let's monkey patch until I can merge this into the main branch

    ;; It seems that there was already an implementation for this, thank
    ;; to https://github.com/emacs-lsp/lsp-mode/pull/2573.

    (lsp-interface (omnisharp:ErrorMessage (:Text :FileName :Line :Column))
                   (omnisharp:ProjectInformationRequest (:FileName))
                   (omnisharp:MsBuildProject (:IsUnitProject :IsExe :Platform :Configuration :IntermediateOutputPath :OutputPath :TargetFrameworks :SourceFiles :TargetFramework :TargetPath :AssemblyName :Path :ProjectGuid))
                   (omnisharp:ProjectInformation (:ScriptProject :MsBuildProject))
                   (omnisharp:CodeStructureRequest (:FileName))
                   (omnisharp:CodeStructureResponse (:Elements))
                   (omnisharp:CodeElement (:Kind :Name :DisplayName :Children :Ranges :Properties))
                   (omnisharp:CodeElementProperties () (:static :accessibility :testMethodName :testFramework))
                   (omnisharp:Range (:Start :End))
                   (omnisharp:RangeList () (:attributes :full :name))
                   (omnisharp:Point (:Line :Column))
                   (omnisharp:RunTestsInClassRequest (:MethodNames :RunSettings :TestFrameworkname :TargetFrameworkVersion :NoBuild :Line :Column :Buffer :FileName))
                   (omnisharp:RunTestResponse (:Results :Pass :Failure :ContextHadNoTests))
                   (omnisharp:TestMessageEvent (:MessageLevel :Message))
                   (omnisharp:DotNetTestResult (:MethodName :Outcome :ErrorMessage :ErrorStackTrace :StandardOutput :StandardError))
                   (omnisharp:MetadataRequest (:AssemblyName :TypeName :ProjectName :VersionNumber :Language))
                   (omnisharp:MetadataResponse (:SourceName :Source))
                   )

    (defconst lsp-csharp--metadata-uri-re
      "^file:///%24metadata%24/Project/\\(.+\\)/Assembly/\\(.+\\)/Symbol/\\(.+\\)\.cs$"
      "Regular expression matching omnisharp's metadata uri.
Group 1 contains the Project name
Group 2 contains the Assembly name
Group 3 contains the Type name")

    (defun lsp-csharp--path->qualified-name (path)
      "Convert PATH to qualified-namespace-like name."
      (replace-regexp-in-string
       (regexp-quote "/")
       "."
       path))

    (defun lsp-csharp--omnisharp-metadata-uri-handler (uri)
      "Handle `file:/(metadata)' uri from omnisharp-roslyn server.
The uri is parsed and then 'o#/metadata' request is issued to retrieve
metadata from the server. A cache file is created on project root dir that
stores this metadata and filename is returned so lsp-mode can display this file."
      (string-match lsp-csharp--metadata-uri-re uri)
      (-when-let* ((project-name (lsp-csharp--path->qualified-name (url-unhex-string (match-string 1 uri))))
                   (assembly-name (lsp-csharp--path->qualified-name (url-unhex-string (match-string 2 uri))))
                   (type-name (lsp-csharp--path->qualified-name (url-unhex-string (match-string 3 uri))))
                   (metadata-req (lsp-make-omnisharp-metadata-request :project-name project-name
                                                                      :assembly-name assembly-name
                                                                      :type-name type-name))
                   (metadata (lsp-request "o#/metadata" metadata-req))
                   ((&omnisharp:MetadataResponse :source-name :source) metadata)
                   (filename (f-join ".cache"
                                     "lsp-csharp"
                                     "metadata"
                                     "Project" project-name
                                     "Assembly" assembly-name
                                     "Symbol" (concat type-name ".cs")))
                   (file-location (expand-file-name filename (lsp--suggest-project-root)))
                   (metadata-file-location (concat file-location ".metadata-uri"))
                   (path (f-dirname file-location)))

        (unless (find-buffer-visiting file-location)
          (unless (file-directory-p path)
            (make-directory path t))

          (with-temp-file metadata-file-location
            (insert uri))

          (with-temp-file file-location
            (insert source)))
        file-location))

    (defun lsp-csharp--uri->path-fn (uri)
      "Custom implementation of lsp--uri-to-path function to glue omnisharp's metadata uri."
      (if (string-match-p lsp-csharp--metadata-uri-re uri)
          (lsp-csharp--omnisharp-metadata-uri-handler uri)
        (lsp--uri-to-path-1 uri))
      )

    (lsp-register-client
     (make-lsp-client :new-connection
                      (lsp-stdio-connection
                       #'(lambda ()
                           (append
                            (list (lsp-csharp--language-server-path) "-lsp")
                            (when lsp-csharp-solution-file
                              (list "-s" (expand-file-name lsp-csharp-solution-file)))))
                       #'(lambda ()
                           (when-let ((binary (lsp-csharp--language-server-path)))
                             (f-exists? binary))))
                      :activation-fn (lsp-activate-on "csharp")
                      :server-id 'omnisharp
                      :priority -1
                      :uri->path-fn #'lsp-csharp--uri->path-fn
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
                                                 ("o#/backgrounddiagnosticstatus" 'ignore))
                      :download-server-fn #'lsp-csharp--omnisharp-download-server)))
  :hook ((vue-web-mode . lsp)
         ;; (typescript-ts-mode . mymy-javascript-mode-lsp-hook)
         ;; (js-ts-mode . mymy-javascript-mode-lsp-hook)
         (typescript-ts-mode . lsp)
         (js-ts-mode . lsp)
         (csharp-ts-mode . lsp)
         (python-ts-mode . lsp)
         (html-mode . lsp)
         (c-mode . lsp)
         (c-ts-mode . lsp)
         ))

(use-package consult-lsp
  :ensure t
  :after (lsp-mode)
  :config
  (define-key lsp-mode-map (kbd "M-<f1>") #'consult-lsp-diagnostics)
  )

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
  (setq lsp-ui-doc-use-childframe t)

  ;; lsp-format-buffer
  )

(use-package dap-mode
  :ensure t
  :init
  (setq dap-netcore-install-dir "/usr/bin/netcoredbg")
  (require 'dap-ui)
  :config

  ;; (setq dap-print-io nil)
  ;; (setq dap-print-io t)
  (require 'dap-python)

  (require 'dap-netcore)
  (require 'dap-php)
  (require 'dap-mouse)
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

(use-package lsp-java
  :ensure t
  :init
  ;; https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.35.0/jdt-language-server-1.35.0-202404251256.tar.gz

  ;; (setq lsp-java-configuration-runtimes '[( :name "JavaSE-8"
  ;;                                           :path "/usr/lib/jvm/java-8-openjdk/"
  ;;                                           :default nil)
  ;;                                         ( :name "JavaSE-11"
  ;;                                           :path "/usr/lib/jvm/java-11-openjdk/"
  ;;                                           :default t)
  ;;                                         ( :name "JavaSE-17"
  ;;                                           :path "/usr/lib/jvm/java-17-openjdk/"
  ;;                                           :default nil)])
  :config
  (add-hook 'java-mode-hook 'lsp)
  (add-hook 'java-ts-mode-hook 'lsp)
  ;; (cl-case system-type
  ;;   (gnu/linux (setenv "JAVA_HOME" "/usr/lib/jvm/java-17-openjdk")))
  )

(use-package lsp-pyright
  :ensure t
  :after (lsp-mode)
  :config
  (add-hook 'python-ts-mode-hook (lambda () (require 'lsp-pyright)))
  (add-hook 'python-ts-mode-hook #'lsp))

(use-package lsp-tailwind
  :disabled
  :ensure (:host github :type git :repo "merrickluo/lsp-tailwindcss")
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  )

(use-package lsp-haskell
  :ensure t
  ;; :defer 5
  :after (lsp-mode)
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

(use-package ormolu
  :ensure t
  :hook (haskell-mode . ormolu-format-on-save-mode)
  :bind
  (:map haskell-mode-map
        ("C-c r" . ormolu-format-buffer)))

(use-package eglot
  :disabled
  :ensure nil
  :config
  ;; (load-file "./elpa/eglot-1.16/eglot.el")
  ;; eglot-server-programs

  ;; Doesn't work on vue, doesn't support workspace/didChangeWorkspaceFolders

  ;; eglot-ignored-server-capabilities

  (add-hook 'python-ts-mode-hook #'eglot-ensure)
  (add-hook 'haskell-mode-hook #'eglot-ensure)
  ;; (add-hook 'csharp-ts-mode-hook #'eglot-ensure)
  (add-hook 'clojure-mode-hook #'eglot-ensure)
  (add-hook 'c-ts-mode-hook #'eglot-ensure)
  )


;; (use-package groovy-emacs-mode

;; )

(use-package php-ts-mode
  :disabled
  :ensure (:host github :type git :repo "emacs-php/php-ts-mode")
  :config
  (add-to-list 'treesit-language-source-alist
               '(php "https://github.com/tree-sitter/tree-sitter-php" "master" "php/src"))
  )

(use-package php-mode
  :disabled
  :init
  (defun mymy-php-mode-hook ()
    (subword-mode 1)
    (setq-local ac-disable-faces '(font-lock-comment-face font-lock-string-face))
    (add-hook 'hack-local-variables-hook 'php-ide-turn-on nil t)
    (general-define-key
     :keymaps 'php-mode-map
     "C-M-e" 'completion-at-point
     )
    (lsp)
    )

  :config
  (gsetq php-mode-coding-style 'psr2
         php-mode-template-compatibility nil
         php-imenu-generic-expression 'php-imenu-generic-expression-simple)

  ;; ;; If you find phpcs to be bothersome, you can disable it.
  ;; (when (require 'flycheck nil)
  ;;   (add-to-list 'flycheck-disabled-checkers 'php-phpmd)
  ;;   (add-to-list 'flycheck-disabled-checkers 'php-phpcs))

  :hook
  (php-mode . mymy-php-mode-hook)
  )


(use-package jq-mode
  :ensure t)

(use-package restclient
  :ensure (restclient :files ("*.el"))
  :after (jq-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))
  (require 'restclient-jq)
  )

;; (use-package ob-restclient
;;   :after org)

;; (use-package gyp
;;   :ensure (:host github :type git :repo "nodejs/node-gyp" :files ("gyp/tools/emacs/*.el"))
;;   )
;;** Kotlin

(use-package kotlin-ts-mode
  :ensure t
  :init
  (add-to-list 'treesit-language-source-alist '(kotlin "https://github.com/fwcd/tree-sitter-kotlin"))
  :config
  (add-to-list 'auto-mode-alist '("\\.kt\\'" . kotlin-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.kts\\'" . kotlin-ts-mode))
  )


;;** Python
(use-package python
  :ensure nil
  :init
  (setq auto-mode-alist (remove '("\\.py[iw]?\\'" . python-mode) auto-mode-alist))
  :config
  (comment
   (dolist (mode-iter '(python-ts-mode))
     (font-lock-add-keywords mode-iter
                             '(("\\([@~^&\|!<>:=\\+*/%-]\\)" 0
                                'fontlock-operator-face keep)
                               ("\\<[\\+-]?[0-9]+\\(.[0-9]+\\)?\\>" 0
                                'font-lock-number-face keep)))))
  (comment
   :hook
   ((python-ts-mode . (highlight-indentation-mode))
    (python-ts-mode . (electric-operator-mode))
    (python-ts-mode . (hs-minor-mode)))))

(use-package blacken
  :ensure t
  :hook (python-mode . blacken-mode)
  :config (setq blacken-line-length 79))

(use-package poetry
  :ensure t
  ;;; TODO: Need to solve the initial lag when opening a pyhton file.
  :config
  (setenv "WORKON_HOME" "~/.cache/pypoetry/virtualenvs/")
  (add-to-list 'display-buffer-alist
               '("\\*poetry\\*"
                 (display-buffer-no-window)
                 ))
  (general-define-key
   "C-c c" 'poetry)
  :hook
  (python-mode . poetry-tracking-mode))

;;** Haskell
(use-package haskell-mode
  :ensure (haskell-mode :host github :type git :repo "haskell/haskell-mode")
  :config
  (define-key haskell-mode-map [f8] 'haskell-navigate-imports)
  (custom-set-variables '(haskell-process-type 'cabal-repl))
  )

(use-package hindent
  :ensure t
  :config
  (add-hook 'haskell-mode-hook #'hindent-mode))


;;** flycheck
(use-package flycheck
  :ensure t
  :config
  (gsetq flycheck-indication-mode 'right-fringe)
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
;;** Magit
(use-package transient
  :ensure t)

(use-package magit
  :after (transient)
  :ensure t
  :defer 5)

(use-package forge
  :ensure t
  :after (magit transient)
  )

(use-package leetcode
  :ensure t
  :config
  (setq leetcode-prefer-language "python3")
  (setq leetcode-prefer-sql "mysql")
  (setq leetcode-save-solutions t)
  (setq leetcode-directory "~/Projects/leetcode")
  (el-patch-defun leetcode--install-my-cookie ()
    "Install leetcode dependencies."
    (let ((async-shell-command-display-buffer t))
      (cond
       ((executable-find "/usr/bin/pacman")
        (async-shell-command
         ;; Be sure to have installed pipx, since this doesn't exist in pacman.
         "pipx install my_cookies"
         (get-buffer-create "*leetcode-install*"))
        )
       (t
        (async-shell-command
         "pip3 install my_cookies"
         (get-buffer-create "*leetcode-install*"))))))
  )

;;** Dired

(use-package dired
  :after hydra
  :ensure nil
  :bind (:map dired-mode-map
              ("." . hydra-dired/body))
  :init
  ;; Viva la emacs
  (setq dired-mouse-drag-files t)                   ; added in Emacs 29
  (setq mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29

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

(use-package dired+
  :ensure nil
  :no-require t
  :config
  (remove-hook 'dired-mode-hook 'diredp--set-up-font-locking))

(use-package diredfl
  :disabled
  :config
  (diredfl-global-mode -1)
  )

(use-package dired-subtree
  :disabled
  :bind (:map dired-mode-map
              ("i" . dired-subtree-toggle)))

(use-package dired-collapse
  :ensure t
  :hook
  ((dired-mode . dired-collapse-mode)))

(use-package dirvish
  :ensure t
  ;;; Why did I disable this?
  ;; :disabled
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
    ("h"   dirvish-history-jump) ; remapped `)describe-mode'
    ("s"   dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
    ("v"   dirvish-vc-menu)      ; remapped `dired-view-file'
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
         ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
         ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
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
;;   :ensure (empv :type git :host github :repo "isamert/empv.el")
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

(use-package openwith
  :disabled
  :init
  (gsetq openwith-associations
         (list
          (list (openwith-make-extension-regexp
                 '("mpg" "mpeg" "mp3" "mp4"
                   "avi" "wmv" "wav" "mov" "flv"
                   "ogm" "ogg" "mkv"))
                "mpv"
                '(file))
          (list (openwith-make-extension-regexp
                 '("xbm" "pbm" "pgm" "ppm" "pnm"
                   "png" "gif" "bmp" "tif" "jpeg" "jpg"))
                "geeqie"
                '(file))
          (list (openwith-make-extension-regexp
                 '("doc" "xls" "ppt" "odt" "ods" "odg" "odp" "docx"))
                "onlyoffice"
                '(file))
          (list (openwith-make-extension-regexp
                 '("pdf" "ps" "ps.gz" "dvi"))
                "sioyek"
                '(file))))
  :config
  (openwith-mode 1))

(use-package org-web-tools
  :ensure t
  :config
  (require 'org-attach)
  (defhydra hydra-org-web-tools (:hint nil :color blue)
    "
^Leave^     ^Archive^             ^Download^
^─────^─────^───────^─────────────^────────^──────────────────
_q_ quit    _o_ Open attachment   _r_ Read url into org buffer
_._ quit    _a_ Add  attachment   _i_ Insert as entry
^^          ^^                    _k_ Insert as org link
"
    ("." nil :color blue)
    ("q" nil :color blue)
    ("o" org-web-tools-archive-view)
    ("a" org-web-tools-archive-attach)
    ("r" org-web-tools-read-url-as-org)
    ("i" org-web-tools-insert-web-page-as-entry)
    ("k" org-web-tools-insert-link-for-url)))

(use-package doct
  :ensure t
  ;; Description: doct is a function that provides an alternative,
  ;; declarative syntax for describing Org capture templates.
  )

(use-package org-contrib
  :ensure t)

;;** Org mode
(use-package org
  :after (org-contrib ryo-modal)
  ;; :ensure (org :type built-in)
  :config
  (ryo-modal-key
   "okc" #'org-agenda-exit
   :mode 'org-agenda-mode
   )
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

  (section "Org define keys (:prefix C-c o)"
    (general-define-key
     :prefix "C-c o"
     "c" 'org-capture
     "w" 'hydra-org-web-tools/body
     "a" 'org-agenda
     ))

  (org-link-set-parameters
   "attach"
   :follow (lambda (filename)
             (thread-last filename
                          (concat mymy-organization-system-directory-attachments)
                          find-file))
   :help-echo "Filename in the `mymy-organization-system-directory-attachments' directory"
   :face '(:foreground "DarkSeaGreen" :underline t))

  ;; Set indentation level one to one with src declaration
  (setq org-edit-src-content-indentation 0)

  (section "Timestmap Org ID"
    ;; Use timmestamps instead of UUID
    (gsetq org-id-method 'ts)
    ;; Default: "%Y%m%dT%H%M%S.%6N"
    (gsetq org-id-ts-format "%Y%m%dT%H%M%S.%9N%Z%z")
    )
  (section "Org attach"
    (require 'org-attach)
    (gsetq org-attach-id-to-path-function-list
           '(org-attach-id-ts-folder-format org-attach-id-uuid-folder-format))
    (gsetq org-attach-directory mymy-organization-system-directory-attachments)
    )

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
  (ryo-modal-major-mode-keys
    'org-agenda-mode
    ("n" org-agenda-next-line)
    ("u" org-agenda-previous-line)
    ("oks" org-agenda-write)
    ("rl" org-toggle-link-display)
    ("ro" org-agenda-open-link))
  )

(use-package edebug-hydra
  :ensure (edebug-hydra :host github :type git :repo "rgrinberg/edebug-hydra")
  )

;;; I STOPPED HERE
(use-package posframe
  :ensure t)

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

(use-package elisp-slime-nav
  :ensure t
  :config
  ;; elisp-slime-nav-describe-elisp-thing-at-point C-c C-d C-d
  (if (s-suffix? "laptop" (system-name))
      (bind-key "C-." 'elisp-slime-nav-find-elisp-thing-at-point
                emacs-lisp-mode-map
                )
    (bind-key "M-." 'elisp-slime-nav-find-elisp-thing-at-point
              emacs-lisp-mode-map))
  :hook
  (emacs-lisp-mode . elisp-slime-nav-mode))

(use-package csv-mode
  :ensure t
  :hook (csv-mode . csv-align-mode)
  :config
  (ryo-modal-major-mode-keys
    'csv-mode
    ("st" csv-align-fields)))

(use-package git-timemachine
  :disabled
  :ensure t
  :after (ryo-modal)
  :config
  (ryo-modal-key "Gt" 'git-timemachine-toggle)
  (ryo-modal-key "GT" 'hydra-git-timemachine/body)
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

(use-package bibtex
  :ensure nil
  :config
  (setq bibtex-autokey-year-length 4))

(use-package org-ref
  :ensure t
  :defer 5
  :config
  ;; (setq bibtex-completion-bibliography (list (concat main-dropbox-dir "Main.bib")))
  (setq bibtex-completion-bibliography (list (concat mymy-bibliography-system-directory "main.bib")))
  (setq bibtex-completion-pdf-field "file"))

(use-package ebib
  :ensure t
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
  (setq ebib-preload-bib-files (list (concat mymy-bibliography-system-directory "main.bib")))
  (setq ebib-bibtex-dialect 'biblatex)
  (setq ebib-file-associations '(("pdf" . "sioyek")
                                 ("ps" . "zathura")))
  (setq ebib-default-directory mymy-bibliography-system-directory)
  (setq ebib-popup-entry-window t)
  ;; (setq ebib-layout 'index-only)
  (setq ebib-layout 'full)
  (setq ebib-reading-list-file (concat mymy-bibliography-system-directory "2024-01-13_reading_list.org"))

  ;; Ebib doesn't need to manage this
  (setq ebib-notes-directory mymy-bibliography-system-directory)
  (setq ebib-notes-storage 'multiple-notes-per-file)
  (setq ebib-notes-default-file "bib_notes.org")
  (setq ebib-file-search-dirs mymy-organization-system-directory-attachments)
  ;; Double %% for org expansions
  (setq ebib-notes-template (string-join
                             '("* lit %%<%%s>%%?"
                               ":PROPERTIES:"
                               ":ID:         %%(org-id-new)"
                               "%K"
                               ":END:"
                               "%T"
                               ""
                               ""
                               "")
                             "\n"))
  (setq ebib-notes-use-org-capture "r")
  (add-to-list 'org-capture-templates
               `("r" "bibliopraphic reference" entry
                 (file+olp ,ebib-notes-default-file "lit notes")
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
  :bind (("C-c n e" . ebib)))

(use-package ebib-biblio
  :disabled
  :after (ebib biblio)
  :bind (:map biblio-selection-mode-map
              ("e" . ebib-biblio-selection-import)))

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

(use-package org-ql
  :ensure t
  :config
  (section "org-ql Queries"
    (defun mymy-org-ql-query-all-backlinks (id)
      `(and
        (todo)
        (link ,id)))
    )

  (defun mymy-org-ql-see-all-backlinks ()
    (interactive)
    (org-ql-search
      (org-agenda-files)
      (mymy-org-ql-query-all-backlinks
       (or (org-id-get) (user-error "No id at point.")))))

  (defun mymy-org-ql-see-all-unfiled-notes ()
    (interactive)
    (org-ql-search
      (org-agenda-files)
      '(tags "notes")
      )
    )

  (general-define-key "C-c n b" 'mymy-org-ql-see-all-backlinks))

(use-package lister
  :ensure t)

;; Looks good
;; Even though this package already have some utilities that i have
;; implemented there are some that are far better done than mine. So
;; with only a little work i can steal some code or make some glue
;; with how i want some things to work
(use-package delve
  :disabled
  :after (org-roam lister)
  ;; For some reason it just break, maybe i will report this
  ;; Found out that the reason is because show/hide outline
  ;; functionality is buggy with lister
  :ensure (delve :host github :type git :repo "publicimageltd/delve")
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

(use-package vulpea
  :disabled
  :ensure (vulpea :type git :host github :repo "d12frosted/vulpea")
  ;; hook into org-roam-db-autosync-mode you wish to enable
  ;; persistence of meta values (see respective section in README too
  ;; find out what meta means)
  :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable))
  ;; (org-roam-db-sync)
  )

(use-package doct-org-roam
  :after (doct)
  :ensure nil
  )

;;** Org roam
(use-package org-roam
  :ensure t
  :after (org el-patch)
  :init
  ;; (require 'org-roam-patch)

  (defvar org-roam-v2-ack t)
  (defun phc-prognify-and-append (form new-form)
    "Wrap FORM in `progn' if needed and append NEW-FORM"
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

  ;; (gsetq org-roam-directory mymy-org-roam-dir)
  (gsetq org-roam-directory mymy-organization-system-directory)
  (gsetq org-roam-completion-everywhere t)
  (gsetq org-tags-exclude-from-inheritance '("Core" "TODO"))
  (gsetq org-roam-extract-new-file-path "%<%Y%m%dT%H%M%S>--${slug}.org")


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

  :config

  (defhydra hydra-org-roam-dailies (:hint nil :exit t)
    "
               Dailies
    ^Capture^       ^Goto^       ^Move^
────^───────^───────^────^───────^────^────────
_t_: Today      _T_: Today      _p_: Prev note
_y_: Yesterday  _Y_: Yesterday  _n_: Next note
_d_: Pick Date  _D_: Pick Date

_._ quit

"
    ("t" org-roam-dailies-capture-today)
    ("y" org-roam-dailies-capture-yesterday)
    ("d" org-roam-dailies-capture-date)

    ("T" org-roam-dailies-goto-today)
    ("Y" org-roam-dailies-goto-yesterday)
    ("D" org-roam-dailies-goto-date)

    ("p" org-roam-dailies-goto-previous-note :exit nil)
    ("n" org-roam-dailies-goto-next-note :exit nil)

    ("." nil :color blue)
    )

  (general-define-key
   :keymaps 'override
   "C-c C-j" 'hydra-org-roam-dailies/body
   "C-c j" 'hydra-org-roam-dailies/body
   )

  (general-define-key
   :prefix "C-c n"
   "f" 'org-roam-node-find
   "i" 'org-roam-node-insert
   "c" 'org-roam-capture
   "j" 'hydra-org-roam-dailies/body
   "o" 'org-id-get-create
   "k" 'mymy-org-roam-get-link
   "l" 'org-roam-buffer-toggle
   )

  (org-roam-db-autosync-enable)
  (setq org-roam-db-node-include-function (lambda () (not (member "ARCHIVE" (org-get-tags)))))
;;; [BEGIN] Org roam graph
  (section "Org roam graph"
    (gsetq org-roam-graph-filetype "svg")
    (gsetq org-roam-graph-executable "dot") ;; Never use neato unless you are going to keep 100 notes or less
    (gsetq org-roam-graph-viewer "/usr/bin/google-chrome-stable")
    (gsetq org-roam-graph-node-extra-config
           '(("id"
              ("style" . "bold,rounded,filled")
              ("fillcolor" . "#000000")
              ("color" . "#15FF00")
              ("fontcolor" . "#00FFFF")
              )
             ("nodesep" . "0.5")
             )
           )
    (gsetq org-roam-graph-edge-extra-config
           '(("dir" . "forward")
             ("weight" . "3")
             )
           )
    (gsetq org-roam-graph-extra-config
           '(("bgcolor" . "snow2") ;; https://graphviz.org/doc/info/colors.html
             ;; ("rank" . "source")
             ("ordering" . "out")
             ("rankdir" . "TB") ;; TBLR Top Bottom Left Rigth
             ("ranksep" . "1")  ;; https://graphviz.org/docs/attrs/ranksep/
             ("sep" . "4")      ;; Always bigger than esep
             ("esep" . "3")     ;; https://graphviz.org/docs/attrs/esep/
             )
           )
    (gsetq org-roam-graph-link-hidden-types '("file" "https" "fuzzy" "http" "gnus"))
    (gsetq org-roam-graph-shorten-titles 'wrap)
    (gsetq org-roam-graph-max-title-length '15))
  ;; [END]
  (gsetq org-roam-dailies-capture-templates
         `(("d" "date" entry
            "* %<%H:%M> %?"
            :target (file+head+olp "%<%Y-%m-%d>.org"
                                   "#+title: %<%Y-%m-%d>\n#+STARTUP: nofold\n"
                                   ("%<%A, %d %B %Y>"))
            :unnarrowed t)))

  ;; Checking documentation of org-roam-capture--convert-template It seems
  ;; this is just a org-capture thing plus org-roam, so org things should
  ;; be valid here
  (gsetq org-roam-capture-templates
         (doct-org-roam
          `(("Default note" :keys "d"
             :type plain
             :file ,org-roam-extract-new-file-path
             :unnarrowed t
             :template ("#+TITLE: ${title}"
                        "- Content, Concept and composition"
                        ""
                        "* Overview"
                        "%?"))
            ("Project" :keys "p"
             :type entry
             :file ,(concat org-directory mymy-org-projects-file)
             :olp ("Projects")
             :before-finalize ,#'mymy-org-roam-get-link
             :template ("* TODO ${title} :project:"
                        ":PROPERTIES:"
                        ":ID: %(org-id-new)"
                        ":END:"
                        ""
                        "- Description"
                        ""
                        "  %?"))
            ("Literature note" :keys "l"
             :type plain
             :file ,org-roam-extract-new-file-path
             :unnarrowed t
             :template ("#+TITLE: ${title}"
                        "#+TAGS: literature"
                        "- Link: "
                        "- Bibliographic reference"
                        "  Author: "
                        "  Title: "
                        "  Year/Date: "
                        "  "
                        "- Content, Concept and composition"
                        ""
                        "* Overview"
                        "%?")))))

  (gsetq org-roam-node-template-prefixes '(("tags" . "#") ("todo" . "t:")))
  (gsetq org-roam-node-formatter nil)
  ;; This is horrible, this avoids the use of
  ;; org-roam-capture-templates for the sake of it
  (comment
   (gsetq org-roam-dailies-capture-templates
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
                                    ("%<%A, %d %B %Y>"))
             :unnarrowed t))))
  ;; https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/

  ;; TODO: Find a way that this works
  ;; (let ((node (org-roam-node-read)))
  ;;   (if (org-roam-node-file node)
  ;;       (let ((mymy-org-roam-visit-node-other-window nil))
  ;;         (org-roam-node-visit node))
  ;;     (org-roam-capture-
  ;;      :node node
  ;;      :templates templates
  ;;      :props '(:finalize find-file))))

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

  ;; [BEGIN] Org roam faces
  (section "Org roam faces"
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
      "Face for hierarchy")

    (defface mymy-org-roam-ref-face
      '((t (:foreground "Dark Orange" :bold t)))
      "org-roam-ref-face face."))
  ;; [END]

  (gsetq org-roam-node-display-template
         (concat
          "${backlinkcount}"
          (propertize "${hierarchy}" 'face 'mymy-org-roam-title)
          " "
          (propertize "(${tags})" 'face 'mymy-org-roam-tag)))

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

  ;; To not forget this exists
  (setq mymy-org-roam-entry-separation-tag ",")

  (defun org-roam-node-insert-immediate (arg &optional args)
    (interactive "P")
    ;; Refactored after watching system crafters video
    (let ((args (cons arg args))
          (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                    '(:immediate-finish t)))))
      (apply #'org-roam-node-insert args)))

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

  ;; [BEGIN] Org roam preview at point
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
  ;; [END]

  (comment
   (setq org-insert-heading-respect-content nil)
   (defun mymy-org-roam-append-node-heading ()
     (interactive)
     (org-insert-heading-respect-content)
     (mymy-insert-unix-date)
     (org-id-get-create)
     (next-line)
     (goto-char (org-element-property :end (org-element-context)))
     (newline)
     (previous-line)))

  ;; [BEGIN] Backlink propagation of TODO state

  (comment
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
   )

  ;; [END]

  ;; This was the intended behaviour, but I changed course and went
  ;; for a general implementation. I'm keeping this just in case.
  (defun mymy-org-roam-mark-done-inside-link ()
    "Mark all nodes in heading as done if current is marked as done"
    ;; Remember to compare using string= because it ignores the text
    ;; properties
    (when (string= "DONE" org-state)
      (mymy-org-roam-propagate-state-to-link-inside-heading)))

  (comment
   (defun mymy-org-move-next-heading (&optional arg)
     (interactive)
     (org-hide-entry)
     (org-forward-heading-same-level (or arg 1))
     (org-show-entry))

   (defun mymy-org-move-prev-heading ()
     (interactive)
     (mymy-org-move-next-heading -1))

   ;; (defun mymy-org-id-roam-create ()
   ;;   (interactive)
   ;;   (org-id-get-create)
   ;;   (mymy-org-roam-update-headline))
   )

  (comment
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
     ("q" nil :color blue)))

  ;; Ryo modal keybindings
  (comment
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

   (defun mymy-insert-unix-date ()
     (interactive)
     (insert (format-time-string "%s")))

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
   (with-eval-after-load 'which-key
     (which-key-add-key-based-replacements
       "Sk" "Dailies"
       "Se" "Refs & bibtex"
       "Sn" "org-roam node"
       "St" "org-roam tags")))
  (comment
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
    ("C-c C-j j" . mymy-org-roam-dailies-today)
    )))

(use-package websocket
  :disabled
  :ensure t)

(use-package simple-httpd
  :disabled
  :ensure t)

(use-package org-roam-ui
  :disabled
  :after (simple-httpd websocket)
  :ensure (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after (org-roam)
  :config
  (require 'org-roam-protocol)
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package org-transclusion
  :ensure t)

(use-package org-roam-bibtex
  :ensure t
  :config
  (setq orb-roam-ref-format 'org-ref-v3)
  (org-roam-bibtex-mode)
  (setq orb-preformat-templates t)
  (setq orb-preformat-keywords '("title" "citekey" "entry-type" "date" "author"))
  )


(use-package asoc
  :ensure (asoc :type git :host github :repo "troyp/asoc.el"))

(use-package org-capture-ref
  :defer 5
  :after (org-roam)
  ;; :ensure (asoc.el :type git :host github :repo "troyp/asoc.el")
  :ensure (org-capture-ref :type git :host github :repo "yantar92/org-capture-ref")
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
  :ensure t
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
  :ensure nil
  :config
  (bind-key "u" nil eww-link-keymap)
  (bind-key "u" nil eww-image-link-keymap)
  (setq shr-width 80)
  :hook
  ((eww-mode . olivetti-mode)))

(use-package gnus
  :ensure nil
  :config
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
  (add-hook 'gnus-summary-mode-hook #'gnus-undo-mode)
  (setq gnus-asynchronous t))

(use-package deft
  :disabled
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
(use-package quickrun :disabled)

;; I would normally take screenshots of org mode and source code and
;; send it through whatsapp
(use-package screenshot
  :ensure (:type git :host github :repo "tecosaur/screenshot")
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
  :ensure t
  :config
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)))

(use-package projectile
  :ensure t
  ;; TODO: Come here later
  :init
  ;; (general-define-key
  ;;  :keymaps 'projectile-mode-map
  ;;  "C-c t" 'projectile-command-map)
  (setq-default projectile-project-search-path `("~/Projects/"
                                                 ,mymy-organization-system-directory
                                                 ("~/.emacs.d" . 1)))

  (setq projectile-completion-system 'auto)
  :config
  (add-to-list 'projectile-project-root-functions
               #'mymy-find-nearest-solution-file t)
  (add-to-list 'projectile-project-root-functions
               #'not-mymy-find-nearest-chsarp-project t)


  :hook
  (after-init . projectile-mode))

(use-package consult-projectile
  :after (consult projectile)
  :ensure t
  :config

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

  (define-key ryo-modal-mode-map (kbd "SPC p") mymy-projectile-map)
  )

(use-package frames-only-mode
  :ensure t
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
  ;; (setq display-buffer-base-action '(display-buffer-in-side-window))
  ;; (edwina-setup-dwm-keys)
  (edwina-mode 1)
  )

(use-package avy
  :disabled
  :ensure t
  :init
  (require 'linum)
  :config
  (defun avy-goto-word-crt-line ()
    "Jump to a word start on the current line only."
    (interactive)
    (avy-with avy-goto-word-0
      (avy-goto-word-0 nil (line-beginning-position) (line-end-position))))

  :config
  (defhydra hydra-avy (:hint nil :color amaranth :exit t)
    "
^Char^   ^Word^    ^line^
^─────^──^────^────^────^──────
_y_ one  _m_ zero  _n_ current
_u_ two  _k_ one   _i_ goto

_._ cancel
"
    ("y" avy-goto-char)
    ("u" avy-goto-char-2)
    ("m" avy-goto-word-0)
    ("k" avy-goto-word-1)
    ("n" avy-goto-word-crt-line)
    ("i" avy-goto-line)
    ("." nil))

  (ryo-modal-keys
    ("I" avy-goto-word-1)
    ("i" hydra-avy/body))
  ;; (ryo-modal-keys ;;; Avy
  ;;   ("I" avy-goto-word-1)
  ;;   ("i" (("y" avy-goto-char)
  ;;         ("u" avy-goto-char-2)
  ;;         ("m" avy-goto-word-0)
  ;;         ("k" avy-goto-word-1)
  ;;         ("n" avy-goto-word-crt-line)
  ;;         ("i" avy-goto-line)
  ;;         ("/" (("m" avy-goto-word-0-below)
  ;;               ("k" avy-goto-word-1-below)))
  ;;         (";" (("m" avy-goto-word-0-above)
  ;;               ("k" avy-goto-word-1-above))))))
  :init
  (setq avy-case-fold-search nil)
  (setq avy-keys '(?n ?e ?i ?k ?y ?m ?u ?c ?r ?s ?t))
  :config
  (set-face-attribute 'avy-lead-face nil
                      :background "#818182" :foreground "#000000")
  (set-face-attribute 'avy-lead-face-0 nil
                      :background "#bdbca6" :foreground "#000000"))
(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?n ?e ?i ?o ?k ?m ?u ?y))
  ;; (ace-window-display-mode)
  (ryo-modal-key "owf" #'ace-window))

(use-package windmove
  :disabled
  :ensure t
  :config
  (windmove-default-keybindings)
  (setq windmove-wrap-around t))

;; Configure Tempel
(use-package tempel
  :ensure t
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

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

  (defun mymy-projectile-get-path-from-root-like-csharp (&optional dir remove-string)
    "Get the current path from root in a csharp way."
    (when-let ((file-name (buffer-file-name)))
      (replace-regexp-in-string
       (regexp-quote "/")
       "."
       (directory-file-name
        (replace-regexp-in-string
         (thread-last (projectile-project-root)
                      expand-file-name
                      directory-file-name
                      file-name-directory
                      regexp-quote)
         ""
         (file-name-directory file-name))))))
  ;; This thing is used in template

  (add-to-list 'tempel-template-sources #'mymy-tempel-add-org-babel-languages))

(use-package yasnippet-snippets
  :ensure t
  :after (yasnippet)
  ;; :disabled
  ;; :defer 5
  )

(use-package sly
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl --dynamic-space-size 1024")
  ;; (setq inferior-lisp-program "/usr/bin/sbcl --dynamic-space-size 2048")
  ;; (setq inferior-lisp-program "/usr/bin/sbcl --dynamic-space-size 4096")
  ;; (setq inferior-lisp-program "/usr/bin/sbcl --dynamic-space-size 8128")
  (setq sly-lisp-implementations
        '((sbcl ("sbcl" "--dynamic-space-size" "1024"))
          ))
  )

(use-package slime-company
  :disabled
  :after (slime company)
  :config (setq slime-company-completion 'fuzzy
                slime-company-after-completion 'slime-company-just-one-space))

(use-package slime
  :disabled
  :init
  (setq slime-lisp-implementations
        '((sbcl . "/usr/bin/sbcl")))
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (slime-setup '(slime-fancy
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
  ;; (lisp-mode . (lambda () (setq-local company-backends (cons 'company-slime company-backends))))
  (lisp-mode . aggressive-indent-mode))

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
  (gsetq dashboard-item-generators
         '((recents   . doom-dashboard-insert-recents-shortmenu)
           (bookmarks . doom-dashboard-insert-bookmark-shortmenu)
           (projects  . doom-dashboard-insert-project-shortmenu)
           (agenda    . doom-dashboard-insert-org-agenda-shortmenu)))
  (gsetq dashboard-items '(projects agenda bookmarks recents)))

(use-package undo-tree
  :ensure t
  :after (ryo-modal)
  :config
  (ryo-modal-key "q /" undo-tree-visualize)
  (ryo-modal-key "?" undo-tree-redo)
  :config
  (global-undo-tree-mode)
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
  :ensure t
  ;; selected-<major-mode>-map for major mode specific keybindings
  ;; (setq selected-org-mode-map (make-sparse-keymap)) example org mode
  :commands selected-minor-mode
  :config
  (setq selected-minor-mode-override t)
  :bind (:map selected-keymap
              ("q" . selected-off)
              ("" . count-words-region)))

(use-package multiple-cursors
  :ensure t)

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
  :ensure t
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

(use-package combobulate
  :disabled
  :ensure t)

(use-package expreg
  :ensure (:type git :host github :repo "casouri/expreg")
  :init
  (ryo-modal-key "\'" #'expreg-expand)
  )

(use-package expand-region
  :disabled
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
  :ensure t
  :config
  (gsetq yequake-frames
         '(("org-capture"
            (buffer-fns . (yequake-org-capture))
            (width . 0.75)
            (height . 0.5)
            (alpha . 0.95)
            (frame-parameters . ((undecorated . t)
                                 (skip-taskbar . t)
                                 (sticky . t))))))
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
  ;; (setq yequake-frames
  ;;       '(("org-roam-dailies-capture-today"
  ;;          (buffer-fns . (yequake-org-roam-dailies-capture-today))
  ;;          (width . 0.75)
  ;;          (height . 0.5)
  ;;          (alpha . 0.8)
  ;;          (frame-parameters . ((undecorated . t)
  ;;                               (sticky . t)
  ;;                               (skip-taskbar . t)
  ;;                               ;; (window-system . x)
  ;;                               )))))
  )

(use-package terminal-here
  :ensure t
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

(use-package eaf
  :disabled
  :ensure nil
  ;; https://github.com/emacs-eaf/emacs-application-framework/discussions/799#discussioncomment-1222055
  ;; :ensure ( eaf
  ;;             :type git
  ;;             :host github
  ;;             :repo "emacs-eaf/emacs-application-framework"
  ;;             :files ("*")
  ;;             :pre-build (("python3" "install-eaf.py" "--install" "pdf-viewer" "--ignore-sys-deps"))
  ;;             )
  :if (file-exists-p (concat user-emacs-directory
                             "elisp/emacs-application-framework/"))
  :preface
  (add-to-list 'load-path
               (concat user-emacs-directory
                       "elisp/emacs-application-framework/"))
  :config
  (require 'eaf-browser)
  (require 'eaf-pdf-viewer)
  (require 'eaf-rss-reader)
  (require 'eaf-org-previewer)
  (require 'eaf-system-monitor)
  (require 'eaf-file-sender)
  (require 'eaf-2048)
  (require 'eaf-vue-demo)
  (require 'eaf-vue-tailwindcss)
  ;; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  (gsetq eaf-browser-continue-where-left-off t)
  (gsetq eaf-browser-enable-adblocker t)
  ;; (gsetq browse-url-browser-function 'eaf-open-browser)
  ;; (defalias 'browse-web #'eaf-open-browser)
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  ;; unbind, see more in the Wiki
  (eaf-bind-key nil "M-q" eaf-browser-keybinding)
  (setq eaf-webengine-pc-user-agent "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
  (with-eval-after-load 'consult
    (defvar consult--source-eaf
      `( :name     "EAF"
         :category buffer
         :narrow   ?e
         :face     consult-buffer
         :history  buffer-name-history
         :state    ,#'consult--buffer-state
         :items    ,(lambda ()
                      (consult--buffer-query :mode 'eaf-mode :as #'buffer-name))))
    (add-to-list 'consult-buffer-sources 'consult--source-eaf 'append))
  )

;; let dir = default-directory
;; modifyProject (\p -> p { projectDirectory = dir })

(use-package emacs
  :ensure nil
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

  (defun ora-ex-point-mark ()
    (interactive)
    (if rectangle-mark-mode
        (exchange-point-and-mark)
      (let ((mk (mark)))
        (rectangle-mark-mode 1)
        (goto-char mk))))

  ;; https://oremacs.com/2015/02/25/rectangle-hydra/
  (defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                       :color pink
                                       :post (deactivate-mark))
    "
  ^_u_^     _d_elete    _s_tring      |\\     ‗,,,--,,‗
_N_   _U_   _o_k        _y_ank       /,`.-'`'   .‗  \-;;,‗
  ^_n_^     ne_w_-copy  _r_eset     |,4-  ) )‗   .;.(  `'-'
^^^^        _e_xchange  _/_undo    '---''(‗/.‗)-'(‗\‗)
^^^^        _._ ok      _p_aste
"
    ("N" backward-char nil)
    ("U" forward-char nil)
    ("u" previous-line nil)
    ("n" next-line nil)
    ;; ora-ex-point-mark
    ("e" rectangle-exchange-point-and-mark nil)
    ("w" copy-rectangle-as-kill nil)
    ("d" delete-rectangle nil)
    ("r" (if (region-active-p)
             (deactivate-mark)
           (rectangle-mark-mode 1)) nil)
    ("y" yank-rectangle nil)
    ("/" undo nil)
    ("s" string-rectangle nil)
    ("p" kill-rectangle nil)
    ("o" nil nil)
    ("." nil nil))

  (global-set-key (kbd "C-x SPC") 'hydra-rectangle/body)
  (global-set-key (kbd "C-v") 'hydra-rectangle/body)
  ;; (ryo-modal-key "V v" 'hydra-rectangle/body)
  )

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setenv "PATH" (let ((current (getenv "PATH"))
                     (new (concat (getenv "HOME") "/.local/bin")))
                 (if current (concat new ":" current) new)))


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
;; Emacs doing one of its shennanigans again, if you subscribe to renaming
;; then this little gremling will do some moving that will trick your
;; subscribers into thinking the file move there, when it is just emacs
;; trickery
(setq backup-by-copying t)

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
;; (unless window-system
;;   (when (getenv "DISPLAY")
;;     (defun xclip-cut-function (text &optional push)
;;       (with-temp-buffer
;;         (insert text)
;;         (call-process-region
;;          (point-min) (point-max)
;;          "xclip" nil 0 nil "-i" "-selection" "clipboard")))
;;     (defun xclip-paste-function ()
;;       (let* ((xclip-output
;;               (shell-command-to-string "xclip -o -selection clipboard"))
;;              (xclip-output (if (string= xclip-output "Error: target STRING not available\n")
;;                                "" xclip-output))
;;              )
;;         (unless (and (string= (car kill-ring) xclip-output) (string= xclip-output "Error: target STRING not available"))
;;           xclip-output)))
;;     (setq interprogram-cut-function 'xclip-cut-function)
;;     (setq interprogram-paste-function 'xclip-paste-function)
;;     (defun mymy-change-to-xclip-when-in-terminal ()
;;       "Use the correct function to interprogram"
;;       (if (display-graphic-p)
;;           (progn
;;             (setq interprogram-cut-function #'gui-select-text)
;;             (setq interprogram-paste-function #'gui-selection-value)
;;             )
;;         (progn
;;           (setq interprogram-cut-function 'xclip-cut-function)
;;           (setq interprogram-paste-function 'xclip-paste-function))
;;         )
;;       )
;;     ;; (mymy-change-to-xclip-when-in-terminal)
;;     ))

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
            (which-function-mode t)))
(add-hook 'after-init-hook #'winner-mode)
;; (add-hook 'prog-mode
;;           (lambda ()
;;             (auto-fill-mode t)))

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

;; (profiler-start 'cpu+mem)

;; Load all functions
(require 'functions)
(ryo-modal-global-mode t)

;; Init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-process-type 'cabal-repl)
 '(safe-local-variable-values
   '((org-tag-alist
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
     (haskell-compiler-type quote stack)
     (haskell-process-type quote stack))))
