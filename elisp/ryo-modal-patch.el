(require 'ryo-modal)

(with-eval-after-load 'el-patch
  (el-patch-defun ryo-modal-key (key target &rest args)
    "Bind KEY to TARGET in `ryo-modal-mode'.

TARGET can be one of:

kbd-string   Pressing KEY will simulate TARGET as a keypress.
command      Calls TARGET interactively.
list         Each element of TARGET is sent to `ryo-modal-key' again, with
             KEY as a prefix key.  ARGS are copied, except for :name.
             :name will be used by `which-key' (if installed) to name
             the prefix key.
keymap       Similarly to list, each keybinding of provided keymap
             is sent to `ryo-modal-key' again with all keyword arguments applied.
             It also works with keymap that bind other keymaps like `ctl-x-map'.
:hydra       If you have hydra installed, a new hydra will be created and
             bound to KEY.  The first element of ARGS should be a list
             containing the arguments sent to `defhydra'.
:hydra+      If you have hydra installed, this will add heads to a preexisting hydra.
             As with the `:hydra' keyword, the first element of ARGS should be a list
             containing the arguments sent to `defhydra+'.

ARGS should be of the form [:keyword option]... if TARGET is a kbd-string
or a command.  The following keywords exist:

:name      A string, naming the binding.  If ommited get name from TARGET.
:exit      If t then exit `ryo-modal-mode' after the command.
:read      If t then prompt for a string to insert after the command.
:mode      If set to a major or minor mode symbol (e.g. 'org-mode) the key will
           only be bound in that mode.
:norepeat  If t then do not become a target of `ryo-modal-repeat'.
:then      Can be a quoted list of additional commands that will be run after
           the TARGET.  These will not be shown in the name of the binding.
           (use :name to give it a nickname).
:first     Similar to :then, but is run before the TARGET.
:mc-all    If t the binding's command will be added to `mc/cmds-to-run-for-all'.
           If 0 the binding's command will be added to `mc/cmds-to-run-once'.

If any ARGS other han :mode, :norepeat or :mc-all are given, a
new command named ryo:<hash>:<name> will be created. This is to
make sure the name of the created command is unique."
    (cond
     ((listp target)
      (when (and (require 'which-key nil t)
                 (plist-get args :name))
        (let ((mode (plist-get args :mode)))
          (if mode
              (let ((map-name (format "ryo-%s-map" mode)))
                (unless (intern-soft map-name)
                  (set (intern map-name) (make-sparse-keymap))
                  (set-keymap-parent (eval (intern map-name))
                                     ryo-modal-mode-map)
                  (add-to-list 'ryo-modal-mode-keymaps mode))
                (define-key (eval (intern map-name)) (kbd key) `(,(plist-get args :name))))
            (define-key ryo-modal-mode-map (kbd key) `(,(plist-get args :name))))))

      (el-patch-wrap 3 0
        (if (memq (car target) '(closure lambda))
            (let* ((mode (plist-get args :mode))
                   (name (intern (concat "ryo" (and mode (format "-%s" mode)) (format "/lambda-%s" key)))))
              (defalias name target)
              (apply #'ryo-modal-key key name args))
          (mapc (lambda (x)
                  ;; Merge :then lists
                  (when (and (plist-get (cddr x) :then)
                             (plist-get args :then))
                    (setf (cddr x) (plist-put (cddr x) :then (append (plist-get (cddr x) :then)
                                                                     (plist-get args :then)))))
                  ;; Merge :first lists
                  (when (and (plist-get (cddr x) :first)
                             (plist-get args :first))
                    (setf (cddr x) (plist-put (cddr x) :first (append (plist-get (cddr x) :first)
                                                                      (plist-get args :first)))))
                  (apply #'ryo-modal-key `(,(concat key " " (car x))
                                           ,@(cdr x)
                                           ,@(org-plist-delete args :name))))
                target))))
     ((and (require 'hydra nil t)
           (equal target :hydra))
      (apply #'ryo-modal-key `(,key ,(eval `(defhydra ,@(car args))) ,@(cdr args))))
     ((and (require 'hydra nil t)
           (equal target :hydra+))
      (apply #'ryo-modal-key `(,key ,(eval `(defhydra+ ,@(car args))) ,@(cdr args))))
     ((and (stringp target) (keymapp (key-binding (kbd target))))
      (let* ((binding (key-binding (kbd target)))
             (map-to-translate (if (symbolp binding) (symbol-function binding) binding)))
        (let ((translated-keymap (ryo-modal--translate-keymap map-to-translate)))
          (apply #'ryo-modal-key `(,key ,translated-keymap ,@args)))))
     ((and (not (stringp target)) (not (symbol-function target)) (boundp target) (keymapp (symbol-value target)))
      (let ((translated-keymap (ryo-modal--translate-keymap (symbol-value target))))
        (apply #'ryo-modal-key `(,key ,translated-keymap ,@args))))
     ((and (not (stringp target)) (keymapp target))
      (let ((translated-keymap (ryo-modal--translate-keymap (symbol-function target))))
        (apply #'ryo-modal-key `(,key ,translated-keymap ,@args))))
     ((and (symbolp target) (not (functionp target)))
      (error "`%s' isn't a function" (symbol-name target)))
     (t
      (let* ((name (or (plist-get args :name)
                       (if (stringp target)
                           target
                         (symbol-name target))))
             (hash (secure-hash 'md5 (format "%s%s" target args)))
             (docs
              (if (stringp target)
                  (if (keymapp (key-binding (kbd target)))
                      (concat "Call keymap " target)
                    (format "%s → %s (`%s')\n\n%s%s"
                            (key-description (kbd key))
                            (key-description (kbd target))
                            (key-binding (kbd target))
                            (documentation (key-binding (kbd target)))
                            (mapconcat #'documentation (plist-get args :then) "\n")))
                (concat (documentation target)
                        (mapconcat #'documentation (plist-get args :then) "\n"))))
             (func
              (cond
               ((thread-first (org-plist-delete args :mode)
                              (org-plist-delete :norepeat)
                              (org-plist-delete :mc-all))
                (eval
                 `(defun ,(intern (concat "ryo:" hash ":" name)) ()
                    ,docs
                    (interactive)
                    (dolist (f (quote ,(plist-get args :first)))
                      (if (commandp f)
                          (let ((real-this-command f))
                            (call-interactively f))
                        (apply f nil)))
                    (if (and (stringp ',target)
                             (keymapp (key-binding (kbd ,target))))
                        (progn
                          (when ,(plist-get args :exit) (ryo-modal-mode -1))
                          (setq unread-command-events (listify-key-sequence (kbd ',target))))
                      (let ((real-this-command
                             (if (stringp ',target)
                                 (key-binding (kbd ,target))
                               ',target)))
                        (call-interactively real-this-command))
                      (dolist (f (quote ,(plist-get args :then)))
                        (if (commandp f)
                            (let ((real-this-command f))
                              (call-interactively f))
                          (apply f nil)))
                      (when ,(plist-get args :exit) (ryo-modal-mode -1))
                      (when ,(plist-get args :read) (insert (read-string "Insert: ")))))))
               ((stringp target)
                (if (keymapp (key-binding (kbd target)))
                    ;; TODO: This doesn't seem to work with "keymaps inside of keymaps"
                    (lambda () (interactive)
                      (setq unread-command-events (listify-key-sequence (kbd target))))
                  (key-binding (kbd target))))
               (t
                target)))
             (mode (plist-get args :mode)))
        (when (plist-get args :norepeat)
          (add-to-list 'ryo-modal--non-repeating-commands func))
        (let ((mc-all (plist-get args :mc-all)))
          (when mc-all
            (if (and (equal mc-all 0) (not (memq func mc/cmds-to-run-for-all)))
                (add-to-list 'mc/cmds-to-run-once func)
              (and (not (memq func mc/cmds-to-run-once))
                   (add-to-list 'mc/cmds-to-run-for-all func)))))
        (if mode
            (let ((map-name (format "ryo-%s-map" mode)))
              (unless (intern-soft map-name)
                (set (intern map-name) (make-sparse-keymap))
                (set-keymap-parent (eval (intern map-name))
                                   ryo-modal-mode-map)
                (add-to-list 'ryo-modal-mode-keymaps mode))
              (define-key (eval (intern map-name)) (kbd key) func))
          (define-key ryo-modal-mode-map (kbd key) func))
        (add-to-list 'ryo-modal-bindings-list `(,key ,name ,@args))))))

  (el-patch-defmacro ryo-modal-major-mode-keys (mode &rest args)
    "Bind several keys in `ryo-modal-mode', but only if major mode is MODE.
ARGS is the same as `ryo-modal-keys'."
    (el-patch-add (declare (indent defun)))
    `(progn
       ,@(mapcar (lambda (x)
                   `(ryo-modal-key ,(car x)
                                   (if ,(stringp (cadr x))
                                       ,(cadr x)
                                     (quote ,(cadr x)))
                                   ,@(nthcdr 2 x)
                                   :mode ,mode))
                 args)))

  (el-patch-defmacro ryo-modal-keys (&rest args)
    "Bind several keys in `ryo-modal-mode'.
Typically each element in ARGS should be of the form (key target [keywords]).
The target should not be quoted.
The first argument may be a list of keywords; they're applied to all keys:

  \(:exit t :then '(kill-region)).

See `ryo-modal-key' for more information."
    (el-patch-add (declare (indent defun)))
    (let ((kw-list
           (if (symbolp (caar args))
               (pop args)
             nil)))
      `(progn
         ,@(mapcar (lambda (x)
                     `(ryo-modal-key ,(car x)
                                     ,(if (stringp (cadr x))
                                          (cadr x)
                                        `(quote ,(cadr x)))
                                     ,@(nthcdr 2 x)
                                     ,@kw-list))
                   args))))
  )

(provide 'ryo-modal-patch)
