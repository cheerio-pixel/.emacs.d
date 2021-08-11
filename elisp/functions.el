(require 'avy)
(require 'elpy)
(require 'subr-x)
(require 'cl-lib)


(defun org-hide-properties ()
  "Hide all org-mode headline property drawers in buffer. Could be slow if it has a lot of overlays."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
      (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put ov_this 'display "")
        (overlay-put ov_this 'hidden-prop-drawer t))))
  (put 'org-toggle-properties-hide-state 'state 'hidden))

(defun org-show-properties ()
  "Show all org-mode property drawers hidden by org-hide-properties."
  (interactive)
  (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
  (put 'org-toggle-properties-hide-state 'state 'shown))

(defun org-toggle-properties ()
  "Toggle visibility of property drawers."
  (interactive)
  (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
      (org-show-properties)
    (org-hide-properties)))


;; https://emacs.stackexchange.com/questions/35417/how-to-insert-strings-from-a-string-list-in-one-file-into-another-file-with-a-h
(defun mymy-common-numbers ()
  (interactive)
  (helm :sources
        (helm-build-in-file-source "Strings"
            "~/Dropbox (Maestral)/Creativè/Common-numbers.txt"
          :action
          '(("Insert vertically" .
             (lambda (_candidate)
               (insert (mapconcat #'identity (helm-marked-candidates) "\n"))))
            ("Insert horizontally" .
             (lambda (_candidate)
               (insert (mapconcat #'identity (helm-marked-candidates) " "))))))))

(defun org-roam-node-insert-immediate (&optional filter-fn)
  "Find an Org-roam node and insert (where the point is) an \"id:\" link to it.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out."
  (interactive)
  (unwind-protect
      ;; Group functions together to avoid inconsistent state on quit
      (atomic-change-group
        (let* (region-text
               beg end
               (_ (when (region-active-p)
                    (setq beg (set-marker (make-marker) (region-beginning)))
                    (setq end (set-marker (make-marker) (region-end)))
                    (setq region-text (org-link-display-format (buffer-substring-no-properties beg end)))))
               (node (org-roam-node-read region-text filter-fn))
               (description (or region-text
                                (org-roam-node-title node))))
          (if (org-roam-node-id node)
              (progn
                (when region-text
                  (delete-region beg end)
                  (set-marker beg nil)
                  (set-marker end nil))
                (insert (org-link-make-string
                         (concat "id:" (org-roam-node-id node))
                         description)))
            (org-roam-capture-
             :node node
             :props (append
                     (when (and beg end)
                       (list :region (cons beg end)))
                     (list :insert-at (point-marker)
                           :link-description description
                           :finalize 'insert-link)))
            (org-capture-finalize)
            )))
    (deactivate-mark))
  )

(defun elpy-goto-definition-or-rgrep ()
  "Go to the definition of the symbol at point, if found. Otherwise, run `elpy-rgrep-symbol'."
  (interactive)
  (ring-insert find-tag-marker-ring (point-marker))
  (condition-case nil (elpy-goto-definition)
    (error (elpy-rgrep-symbol
            (concat "\\(def\\|class\\)\s" (thing-at-point 'symbol) "(")))))
(defun pipenv-start-first-time ()
  "My set of command to run when i want to create a virtual env"
  (interactive)
  (pipenv-lock)
  (pipenv-shell)
  )
(defun my-save-word ()
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))
(defun enable-jedi()
  (setq-local company-backends
              (append '(company-jedi company-anaconda) company-backends)))
(defun my/refile (file headline)
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile nil nil (list headline file nil pos))))
(defun +org-toggle-inline-image-at-point ()
  "Toggle inline image at point."
  (interactive)
  (if-let* ((bounds (and (not org-inline-image-overlays)
                         (org-in-regexp org-link-any-re nil t)))
            (beg (car bounds))
            (end (cdr bounds)))
      (org-display-inline-images nil nil beg end)
    (org-toggle-inline-images)))

(defun yequake-org-roam-dailies-capture-today (&optional goto)
  "Call `org-roam-dailies-capture-today' in a Yequake frame.
  Adds a function to `org-capture-after-finalize-hook' that closes
  the recently toggled Yequake frame and removes itself from the
  hook.

  Note: if another Yequake frame is toggled before the capture is
  finalized, when the capture is finalized, the wrong Yequake frame
  will be toggled."
  (let* ((remove-hook-fn (lambda ()
                           (remove-hook 'org-capture-after-finalize-hook #'yequake-retoggle))))
    (add-hook 'org-capture-after-finalize-hook remove-hook-fn)
    (add-hook 'org-capture-after-finalize-hook #'yequake-retoggle)
    ;; MAYBE: Propose an `org-capture-switch-buffer-fn' variable that could be rebound here.

    ;; NOTE: We override `org-switch-to-buffer-other-window' because
    ;; it always uses `switch-to-buffer-other-window', and we want to
    ;; display the template menu and capture buffer in the existing
    ;; window rather than splitting the frame.
    (cl-letf* (((symbol-function #'org-switch-to-buffer-other-window)
                (symbol-function #'switch-to-buffer)))
      (condition-case nil
          (progn
            (org-roam-dailies-capture-today goto)
            ;; Be sure to return the "CAPTURE-" buffer, which is the current
            ;; buffer at this point.
            (current-buffer))
        ((error quit)
         ;; Capture aborted: remove the hook and hide the capture frame.
         (remove-hook 'org-capture-after-finalize-hook #'yequake-retoggle)
         (yequake-retoggle))))))

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
  (when killed-file-list
    (find-file (pop killed-file-list))))
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

(defun org-goto-tasks()
  (interactive)
  (org-id-goto "2271da12-1a80-4627-be66-9678d3926a36")
  )
(defun org-goto-school-schedule ()
  (interactive)
  (org-id-goto "d3b993f2-6132-422c-8b30-ce2ef1867235")
  )


(defun strip-<p>-html (paragraph contents info)
  (string-remove-suffix
   "</p>"
   (string-remove-prefix
    "<p>"
    (org-html-paragraph paragraph contents info)))
  )

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(defun shift-text (distance)
  (if (use-region-p)
      (let ((mark (mark)))
        (save-excursion
          (indent-rigidly (region-beginning)
                          (region-end)
                          distance)
          (push-mark mark t t)
          (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    distance)))

(defun shift-right (count)
  (interactive "p")
  (shift-text count))

(defun shift-left (count)
  (interactive "p")
  (shift-text (- count)))
;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(defun paste-primary-selection ()
  (interactive)
  (insert (gui-get-primary-selection)))

(defun yank-at-point ()
  (interactive)
  (yank)
  (pop-global-mark)
  )
(defun mymy/insert-pair$ ()
  (interactive)
  (save-excursion
    (insert "$")
    (exchange-point-and-mark)
    (insert "$")
    )
  )
(defun mymy/insert-pair~ ()
  (interactive)
  (save-excursion
    (insert "~")
    (exchange-point-and-mark)
    (insert "~")
    )
  )
(defun mymy/insert-pair= ()
  (interactive)
  (save-excursion
    (insert "=")
    (exchange-point-and-mark)
    (insert "=")
    )
  )

(defun avy-delete-region (arg)
  "Select two lines and delete the region between them.

  The window scope is determined by `avy-all-windows' or
  `avy-all-windows-alt' when ARG is non-nil."
  (interactive "P")
  (let ((initial-window (selected-window)))
    (avy-with avy-delete-region
      (let* ((beg (save-selected-window
                    (list (avy--line arg) (selected-window))))
             (end (list (avy--line arg) (selected-window))))
        (cond
         ((not (numberp (car beg)))
          (user-error "Fail to select the beginning of region"))
         ((not (numberp (car end)))
          (user-error "Fail to select the end of region"))
         ;; Restrict operation to same window. It's better if it can be
         ;; different windows but same buffer; however, then the cloned
         ;; buffers with different narrowed regions might cause problem.
         ((not (equal (cdr beg) (cdr end)))
          (user-error "Selected points are not in the same window"))
         ((< (car beg) (car end))
          (save-excursion
            (delete-region
             (car beg)
             (progn (goto-char (car end)) (forward-visible-line 1) (point)))))
         (t
          (save-excursion
            (delete-region
             (progn (goto-char (car beg)) (forward-visible-line 1) (point))
             (car end)))))))
    (select-window initial-window)))
(defun dwim/org-clock-get-string ()
  (let ((clock-string (org-clock-get-clock-string))
        (help-text "Org mode clock is running.\nmouse-1 shows a \
  menu\nmouse-2 will jump to task"))
    (if (and (> org-clock-string-limit 0)
             (> (length clock-string) org-clock-string-limit))
        (propertize
         (substring clock-string 0 org-clock-string-limit)
         'help-echo (concat help-text ": " org-clock-heading))
      (propertize clock-string 'help-echo help-text)))
  )
(defun avy-delete-whole-line (arg)
  "Select line and delete the whole selected line.

  With a numerical prefix ARG, delete ARG line(s) starting from the
  selected line.  If ARG is negative, delete backward.

  If ARG is zero, delete the selected line but exclude the trailing
  newline.

  \\[universal-argument] 3 \\[avy-kil-whole-line] delete three lines
  starting from the selected line.  \\[universal-argument] -3

  \\[avy-delete-whole-line] delete three lines backward including the
  selected line."
  (interactive "P")
  (let ((initial-window (selected-window)))
    (avy-with avy-delete-whole-line
      (let* ((start (avy--line)))
        (if (not (numberp start))
            (user-error "Fail to select the line to delete")
          (save-excursion (goto-char start)
                          (my-delete-whole-line arg)
                          ))))
    (select-window initial-window)))

(defun replace-next-line ()
  "Replace the next line with kill ring."
  (interactive)
  (save-excursion
    (forward-line)
    (beginning-of-line)
    (delete-line 1)
    (yank)
    )
  )
(defun replace-current-line()
  "Replace current line with kill ring"
  (interactive)
  (beginning-of-line)
  (delete-line 1)
  (yank)
  )
(defun insert-quotes (&optional arg)
  "Enclose following ARG sexps in string.
  Leave point after open-paren.
  A negative ARG encloses the preceding ARG sexps instead.
  No argument is equivalent to zero: just insert `\"\"' and leave point between.
  If `string-require-spaces' is non-nil, this command also inserts a space
  before and after, depending on the surrounding characters.
  If region is active, insert enclosing characters at region boundaries.

  This command assumes point is not in a string or comment."
  (interactive "P")
  (insert-pair arg ?\" ?\"))

;;(advice-add 'toggle-input-method :after 'ryo-modal-global-mode)
;;;(advice-add 'toggle-input-method :after 'ryo-modal-global-mode)


(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))
(defun create-scratch-buffer nil
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  )

;;;;;                   AVY
(defun avy-goto-word-crt-line ()
  "Jump to a word start on the current line only."
  (interactive)
  (avy-with avy-goto-word-0
    (avy-goto-word-0 nil (line-beginning-position) (line-end-position))))

(defun avy-goto-parens ()
  (interactive)
  (let ((avy-command this-command))   ; for look up in avy-orders-alist
    (avy-jump "(+")))
(defun avy-org-same-level (&optional all)
  "Go to any org heading of the same level as the current one.

  By default, choices are limited to headings under common
  subheading, but if called with a prefix argument, will be
  buffer-global."
  (interactive "P")
  (let ((org-level (org-current-level)))
    (avy--generic-jump
     (format "^%s "
             (regexp-quote
              (make-string org-level ?*)))
     nil
     'pre
     (unless (or all (= org-level 1))
       (save-excursion
         (outline-up-heading 1)
         (point)))
     (unless (or all (= org-level 1))
       (save-excursion
         (outline-up-heading 1)
         (org-end-of-subtree))))))

(defun avy-org-parent-level (&optional all)
  "Go to any org heading one level above the current one.


  By default, choices are limited to headings under common
  subheading, but if called with a prefix argument, will be
  buffer-global."
  (interactive "P")
  (let ((org-level (org-current-level)))
    (if (= org-level 1)
        (message "Already at top level.")
      (avy--generic-jump
       (format "^%s "
               (regexp-quote
                (make-string (- org-level 1) ?*)))
       nil
       'pre
       (unless (or all (= org-level 2))
         (save-excursion
           (outline-up-heading 2)
           (point)))
       (unless (or all (= org-level 2))
         (save-excursion
           (outline-up-heading 2)
           (org-end-of-subtree)))))))

(defun avy-org-child-level (&optional all)
  "Go to any org heading one level below the current one.

  By default, choices are limited to headings under common
  subheading, but if called with a prefix argument, will be
  buffer-global."
  (interactive "P")
  (if (save-excursion (org-goto-first-child))
      (let ((org-level (org-current-level)))
        (avy--generic-jump
         (format "^%s "
                 (regexp-quote
                  (make-string (+ org-level 1) ?*)))
         nil
         'pre
         (unless all
           (save-excursion
             (ignore-errors
               (outline-up-heading 0))
             (point)))
         (unless all
           (save-excursion
             (ignore-errors
               (outline-up-heading 0))
             (org-end-of-subtree)))))
    (message "Heading has no children.")))

(defun avy-org-goto-level (&optional num)
  "Prompt for an org level to go to, defaulting to the current one."
  (interactive (list
                (read-number "Select heading level: " (org-current-level))))
  (avy--generic-jump
   (format "^%s " (regexp-quote (make-string num ?*)))
   nil
   'pre))

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


(defun smart-open-line ()
  "Insert an empty line after the current line.
  Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))


(defun my-buffer-predicate (buffer)
  (if (string-match "helm" (buffer-name buffer))
      nil
    t))
(set-frame-parameter nil 'buffer-predicate 'my-buffer-predicate)

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
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    )) ; end-of-let
;; put the point in the lowest line and return
;; (next-line arg)

(defun avy-goto-word-forward-crt-line ()
  (interactive)
  (avy-goto-word-crt-line)
  (forward-word)
  )

;;;;;CURSOR
(defvar hcz-set-cursor-color-color "")
(defvar hcz-set-cursor-color-buffer "")
(defun hcz-set-cursor-color-according-to-mode ()
  "Change cursor color according to some minor modes."
  ;; set-cursor-color is somewhat costly, so we only call it when needed:
  (let ((color
         (if buffer-read-only "yellow"
           (if overwrite-mode "red"
             (if ryo-modal-mode "white"
               "cyan")))))
    (unless (and
             (string= color hcz-set-cursor-color-color)
             (string= (buffer-name) hcz-set-cursor-color-buffer))
      (set-cursor-color (setq hcz-set-cursor-color-color color))
      (setq hcz-set-cursor-color-buffer (buffer-name)))))

;;;;;DELETE FUNCTIONS
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

(defvar movement-syntax-table
  (let ((st (make-syntax-table)))
    ;; ` default = punctuation
    ;; ' default = punctuation
    ;; , default = punctuation
    ;; ; default = punctuation
    (modify-syntax-entry ?{ "." st)  ;; { = punctuation
    (modify-syntax-entry ?} "." st)  ;; } = punctuation
    (modify-syntax-entry ?\" "." st) ;; " = punctuation
  (modify-syntax-entry ?\\ "_" st) ;; \ = symbol
  (modify-syntax-entry ?\$ "_" st) ;; $ = symbol
  (modify-syntax-entry ?\% "_" st) ;; % = symbol
  st)
  "Syntax table used while executing custom movement functions.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;WINDOW;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hl-line-update-face (window)
  "Update the `hl-line' face in WINDOW to
  indicate whether the window is selected."
  (with-current-buffer (window-buffer window)
    (when hl-line-mode
      (if (eq (current-buffer) (window-buffer (selected-window)))
          (face-remap-reset-base 'hl-line)
        (face-remap-set-base 'hl-line
                             (face-all-attributes 'hl-line-inactive))))))
                                        ;Here ends what i have behind me


(defun split-window-sensibly-prefer-horizontal (&optional window) ;; Important
  "Based on split-window-sensibly, but designed to prefer a horizontal split,
i.e. windows tiled side-by-side."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally
             (with-selected-window window
               (split-window-right)))
        (and (window-splittable-p window)
             ;; Split window vertically
             (with-selected-window window
               (split-window-below)))
        (and
         ;; If WINDOW is the only usable window on its frame (it is
         ;; the only one or, not being the only one, all the other
         ;; ones are dedicated) and is not the minibuffer window, try
         ;; to split it horizontally disregarding the value of
         ;; `split-height-threshold'.
         (let ((frame (window-frame window)))
           (or
            (eq window (frame-root-window frame))
            (catch 'done
              (walk-window-tree (lambda (w)
                                  (unless (or (eq w window)
                                              (window-dedicated-p w))
                                    (throw 'done nil)))
                                frame)
              t)))
         (not (window-minibuffer-p window))
         (let ((split-width-threshold 0))
           (when (window-splittable-p window t)
             (with-selected-window window
               (split-window-right))))))))
(defun split-window-really-sensibly (&optional window)
  (let ((window (or window (selected-window))))
    (if (> (window-total-width window) (* 2 (window-total-height window)))
        (with-selected-window window (split-window-sensibly-prefer-horizontal window))
      (with-selected-window window (split-window-sensibly window)))))


(defun delete-word-or-whitespace (&optional arg)
  "http://stackoverflow.com/a/20456861/2112489"
  (interactive "P")
  (with-syntax-table movement-syntax-table
    (let* (
           beg
           end
           (word-regexp "\\sw")
           (punctuation-regexp "\\s.")
           (symbol-regexp "\\s_\\|\\s(\\|\\s)"))
      (cond
       ;; Condition # 1
       ;; right of cursor = word or punctuation or symbol
       ((or
         (save-excursion (< 0 (skip-syntax-forward "w")))
         (save-excursion (< 0 (skip-syntax-forward ".")))
         (save-excursion (< 0 (skip-syntax-forward "_()"))))
        ;; Condition #1 -- Step 1 of 2
        (cond
         ;; right of cursor = word
         ((save-excursion (< 0 (skip-syntax-forward "w")))
          (skip-syntax-forward "w")
          (setq end (point))
          (while (looking-back word-regexp)
            (backward-char))
          (setq beg (point))
          (delete-region beg end))
         ;; right of cursor = punctuation
         ((save-excursion (< 0 (skip-syntax-forward ".")))
          (skip-syntax-forward ".")
          (setq end (point))
          (while (looking-back punctuation-regexp)
            (backward-char))
          (setq beg (point))
          (delete-region beg end))
         ;; right of cursor = symbol
         ((save-excursion (< 0 (skip-syntax-forward "_()")))
          (skip-syntax-forward "_()")
          (setq end (point))
          (while (looking-back symbol-regexp)
            (backward-char))
          (setq beg (point))
          (delete-region beg end)))
        ;; Condition #1 -- Step 2 of 2
        (cond
         ;; right of cursor = whitespace
         ;; left of cursor = not word / not symbol / not punctuation = whitespace or bol
         ((and
           (save-excursion (< 0 (skip-chars-forward "\s\t")))
           (not (save-excursion (> 0 (skip-syntax-backward "w"))))
           (not (save-excursion (> 0 (skip-syntax-backward "."))))
           (not (save-excursion (> 0 (skip-syntax-backward "_()")))))
          (setq beg (point))
          (skip-chars-forward "\s\t")
          (setq end (point))
          (delete-region beg end))
         ;; right of cursor = whitespace
         ;; left of cursor = word or symbol or punctuation
         ((and
           (save-excursion (< 0 (skip-chars-forward "\s\t")))
           (or
            (save-excursion (> 0 (skip-syntax-backward "w")))
            (save-excursion (> 0 (skip-syntax-backward ".")))
            (save-excursion (> 0 (skip-syntax-backward "_()")))))
          (fixup-whitespace))))
       ;; Condition # 2
       ;; right of cursor = whitespace
       ;; left of cursor = bol | left of cursor = whitespace | right of cursor = whitespace + eol
       ((and
         (save-excursion (< 0 (skip-chars-forward "\s\t")))
         (or
          (bolp)
          (save-excursion (> 0 (skip-chars-backward "\s\t")))
          (save-excursion (< 0 (skip-chars-forward "\s\t")) (eolp))))
        (setq beg (point))
        (skip-chars-forward "\s\t")
        (setq end (point))
        (delete-region beg end))
       ;; Condition # 3
       ;; right of cursor = whitespace or eol
       ;; leFT of cursor = word or symbol or punctuation
       ;; not bol + word or symbol or punctuation
       ;; not bol + whitespace + word or symbol or punctuation
       ((and
         (or (save-excursion (< 0 (skip-chars-forward "\s\t"))) (eolp))
         (or
          (save-excursion (> 0 (skip-syntax-backward "w")))
          (save-excursion (> 0 (skip-syntax-backward ".")))
          (save-excursion (> 0 (skip-syntax-backward "_()"))))
         (not (save-excursion (> 0 (skip-syntax-backward "w")) (bolp)))
         (not (save-excursion (> 0 (skip-syntax-backward ".")) (bolp)))
         (not (save-excursion (> 0 (skip-syntax-backward "_()")) (bolp)))
         (not (save-excursion (and (> 0 (skip-syntax-backward "w")) (> 0 (skip-chars-backward "\s\t")) (bolp))))
         (not (save-excursion (and (> 0 (skip-syntax-backward ".")) (> 0 (skip-chars-backward "\s\t")) (bolp))))
         (not (save-excursion (and (> 0 (skip-syntax-backward "_()")) (> 0 (skip-chars-backward "\s\t")) (bolp)))))
        (setq end (point))
        (cond
         ((save-excursion (> 0 (skip-syntax-backward "w")))
          (while (looking-back word-regexp)
            (backward-char)))
         ((save-excursion (> 0 (skip-syntax-backward ".")))
          (while (looking-back punctuation-regexp)
            (backward-char)))
         ((save-excursion (> 0 (skip-syntax-backward "_()")))
          (while (looking-back symbol-regexp)
            (backward-char))))
        (setq beg (point))
        (when (save-excursion (> 0 (skip-chars-backward "\s\t")))
          (skip-chars-backward "\s\t")
          (setq beg (point)))
        (delete-region beg end)
        (skip-chars-forward "\s\t"))
       ;; Condition # 4
       ;; not bol = eol
       ;; left of cursor = bol + word or symbol or punctuation | bol + whitespace + word or symbol or punctuation
       ((and
         (not (and (bolp) (eolp)))
         (or
          (save-excursion (> 0 (skip-syntax-backward "w")) (bolp))
          (save-excursion (> 0 (skip-syntax-backward ".")) (bolp))
          (save-excursion (> 0 (skip-syntax-backward "_()")) (bolp))
          (save-excursion (and (> 0 (skip-syntax-backward "w")) (> 0 (skip-chars-backward "\s\t")) (bolp)))
          (save-excursion (and (> 0 (skip-syntax-backward ".")) (> 0 (skip-chars-backward "\s\t")) (bolp)))
          (save-excursion (and (> 0 (skip-syntax-backward "_()")) (> 0 (skip-chars-backward "\s\t")) (bolp)))))
        (SKIP-CHARS-forward "\s\t")
        (setq end (point))
        (setq beg (point-at-bol))
        (delete-region beg end))
       ;; Condition # 5
       ;; point = eol
       ;; not an empty line
       ;; whitespace to the left of eol
       ((and
         (not (and (bolp) (eolp)))
         (eolp)
         (save-excursion (> 0 (skip-chars-backward "\s\t"))))
        (setq end (point))
        (skip-chars-backward "\s\t")
        (setq beg (point))
        (delete-region beg end))
       ;; Condition # 6
       ;; point = not eob
       ;; point = bolp and eolp
       ;; universal argument = C-u = '(4)
       ((and
         (not (eobp))
         (and (bolp) (eolp))
         (equal arg '(4)))
        (delete-forward-char 1))) )))
(provide 'functions)
