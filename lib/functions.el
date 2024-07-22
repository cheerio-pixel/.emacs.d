;; -*- lexical-binding: t; -*-

(defmacro comment (&rest _body)
  "The clojure macro for commenting out multiple lines"
  nil)

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
                    ;; file-name-directory ;; With the addition of solutions this should work correctly
                    regexp-quote)
       ""
       (file-name-directory file-name))))))

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

(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

(provide 'functions.el)
