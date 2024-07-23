;; (require 'avy)
;; (require 'elpy)
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

(defvar mymy/org-tag-kill-ring nil
  "This variable will only hold you one set of tags")

;; The body of this functions comes from the function org-roam-tag-add
(defun mymy/org-roam-get-tags ()
  "Get tags at point"
  (if (= (org-outline-level) 0)
      (split-string (or (cadr (assoc "FILETAGS" (org-collect-keywords '("filetags"))))
                        "")
                    ":" 'omit-nulls)
    (org-set-tags (seq-uniq (append tags (org-get-tags))))))


(defun mymy/org-copy-tags ()
  (interactive)
  (setq mymy/org-tag-kill-ring (mymy/org-roam-get-tags)))

(defun mymy/org-roam-paste-tags ()
  (interactive)
  (if mymy/org-tag-kill-ring
      (org-roam-tag-add mymy/org-tag-kill-ring)
    (message "No tags")))

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

(defun sum-time (&rest time)
  (let* ((first-minute (floor (* 100 (apply '+ time))))
         (minutes (% first-minute 60))
         (hours (/ first-minute 60)))
    )
  )


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


(defun new-line-dwim ()
  (interactive)
  (let ((break-open-pair (or (and (looking-back "{") (looking-at "}"))
                             (and (looking-back ">") (looking-at "<"))
                             (and (looking-back "(") (looking-at ")"))
                             (and (looking-back "\\[") (looking-at "\\]")))))
    (cond
     ((save-excursion (comment-beginning))
      (call-interactively #'default-indent-new-line)
      )
     (t
      (newline)
      (when break-open-pair
        (save-excursion
          (newline-and-indent)))
      (indent-for-tab-command)))))

(defun mymy/elpy-nav-backward-block ()
  "Move to the previous line indented like point.
This will skip over lines and statements with different
indentation levels."
  (interactive "^")
  (let ((indent (current-column))
        (start (point))
        (cur nil))
    (when (/= (% indent 1)
              0)
      (setq indent 1))
    (while (and (< indent (current-indentation))
                (not (bobp)))
      (when (equal (point) cur)
        (error "Statement does not start"))
      (setq cur (point)))
    (when (< (current-indentation)
             indent)
      (goto-char start))))

(defun mymy/elpy-nav-forward-indent ()
  "Move forward to the next indent level, or over the next word."
  (interactive "^")
  (if (< (current-column) (current-indentation))
      (let* ((current (current-column))
             (next 1))
        (goto-char (+ (point-at-bol)
                      next)))
    (let ((eol (point-at-eol)))
      (forward-word)
      (when (> (point) eol)
        (goto-char (point-at-bol))))))

(defun mymy/elpy-nav-move-line-or-region-down (&optional beg end)
  "Move the current line or active region down."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list nil nil)))
  (if beg
      (mymy/elpy--nav-move-region-vertically beg end 1)
    (mymy/elpy--nav-move-line-vertically 1)))

(defun mymy/elpy-nav-move-line-or-region-up (&optional beg end)
  "Move the current line or active region down."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list nil nil)))
  (if beg
      (mymy/elpy--nav-move-region-vertically beg end -1)
    (mymy/elpy--nav-move-line-vertically -1)))

(defun mymy/elpy--nav-move-region-vertically (beg end dir)
  "Move the current region vertically in direction DIR."
  (let* ((point-before-mark (< (point) (mark)))
         (beg (save-excursion
                (goto-char beg)
                (point-at-bol)))
         (end (save-excursion
                (goto-char end)
                (if (bolp)
                    (point)
                  (point-at-bol 2))))
         (region (delete-and-extract-region beg end)))
    (goto-char beg)
    (forward-line dir)
    (save-excursion
      (insert region))
    (if point-before-mark
        (set-mark (+ (point)
                     (length region)))
      (set-mark (point))
      (goto-char (+ (point)
                    (length region))))
    (setq deactivate-mark nil)))

(defun mymy/elpy--nav-move-line-vertically (dir)
  "Move the current line vertically in direction DIR."
  (let* ((beg (point-at-bol))
         (end (point-at-bol 2))
         (col (current-column))
         (region (delete-and-extract-region beg end)))
    (forward-line dir)
    (save-excursion
      (insert region))
    (goto-char (+ (point) col))))

(defun enable-jedi()
  (setq-local company-backends
              (append '(company-jedi company-anaconda) company-backends)))

(defun my/refile (file headline)
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile nil nil (list headline file nil pos))))

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

(defun strip-<p>-html (paragraph contents info)
  (thread-last (org-html-paragraph paragraph contents info)
               (string-remove-prefix "<p>")
               (string-remove-suffix "</p>")))

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


;; (defun avy-delete-region (arg)
;;   "Select two lines and delete the region between them.

;;   The window scope is determined by `avy-all-windows' or
;;   `avy-all-windows-alt' when ARG is non-nil."
;;   (interactive "P")
;;   (let ((initial-window (selected-window)))
;;     (avy-with avy-delete-region
;;       (let* ((beg (save-selected-window
;;                     (list (avy--line arg) (selected-window))))
;;              (end (list (avy--line arg) (selected-window))))
;;         (cond
;;          ((not (numberp (car beg)))
;;           (user-error "Fail to select the beginning of region"))
;;          ((not (numberp (car end)))
;;           (user-error "Fail to select the end of region"))
;;          ;; Restrict operation to same window. It's better if it can be
;;          ;; different windows but same buffer; however, then the cloned
;;          ;; buffers with different narrowed regions might cause problem.
;;          ((not (equal (cdr beg) (cdr end)))
;;           (user-error "Selected points are not in the same window"))
;;          ((< (car beg) (car end))
;;           (save-excursion
;;             (delete-region
;;              (car beg)
;;              (progn (goto-char (car end)) (forward-visible-line 1) (point)))))
;;          (t
;;           (save-excursion
;;             (delete-region
;;              (progn (goto-char (car beg)) (forward-visible-line 1) (point))
;;              (car end)))))))
;;     (select-window initial-window)))
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
;; (defun avy-delete-whole-line (arg)
;;   "Select line and delete the whole selected line.

;;   With a numerical prefix ARG, delete ARG line(s) starting from the
;;   selected line.  If ARG is negative, delete backward.

;;   If ARG is zero, delete the selected line but exclude the trailing
;;   newline.

;;   \\[universal-argument] 3 \\[avy-kil-whole-line] delete three lines
;;   starting from the selected line.  \\[universal-argument] -3

;;   \\[avy-delete-whole-line] delete three lines backward including the
;;   selected line."
;;   (interactive "P")
;;   (let ((initial-window (selected-window)))
;;     (avy-with avy-delete-whole-line
;;       (let* ((start (avy--line)))
;;         (if (not (numberp start))
;;             (user-error "Fail to select the line to delete")
;;           (save-excursion (goto-char start)
;;                           (my-delete-whole-line arg)
;;                           ))))
;;     (select-window initial-window)))


;;(advice-add 'toggle-input-method :after 'ryo-modal-global-mode)
;;;(advice-add 'toggle-input-method :after 'ryo-modal-global-mode)

;;;;;                   AVY

;; (defun avy-goto-parens ()
;;   (interactive)
;;   (let ((avy-command this-command))   ; for look up in avy-orders-alist
;;     (avy-jump "(+")))

;; (defun avy-org-same-level (&optional all)
;;   "Go to any org heading of the same level as the current one.

;;   By default, choices are limited to headings under common
;;   subheading, but if called with a prefix argument, will be
;;   buffer-global."
;;   (interactive "P")
;;   (let ((org-level (org-current-level)))
;;     (avy--generic-jump
;;      (format "^%s "
;;              (regexp-quote
;;               (make-string org-level ?*)))
;;      nil
;;      'pre
;;      (unless (or all (= org-level 1))
;;        (save-excursion
;;          (outline-up-heading 1)
;;          (point)))
;;      (unless (or all (= org-level 1))
;;        (save-excursion
;;          (outline-up-heading 1)
;;          (org-end-of-subtree))))))

;; (defun avy-org-parent-level (&optional all)
;;   "Go to any org heading one level above the current one.


;;   By default, choices are limited to headings under common
;;   subheading, but if called with a prefix argument, will be
;;   buffer-global."
;;   (interactive "P")
;;   (let ((org-level (org-current-level)))
;;     (if (= org-level 1)
;;         (message "Already at top level.")
;;       (avy--generic-jump
;;        (format "^%s "
;;                (regexp-quote
;;                 (make-string (- org-level 1) ?*)))
;;        nil
;;        'pre
;;        (unless (or all (= org-level 2))
;;          (save-excursion
;;            (outline-up-heading 2)
;;            (point)))
;;        (unless (or all (= org-level 2))
;;          (save-excursion
;;            (outline-up-heading 2)
;;            (org-end-of-subtree)))))))

;; (defun avy-org-child-level (&optional all)
;;   "Go to any org heading one level below the current one.

;;   By default, choices are limited to headings under common
;;   subheading, but if called with a prefix argument, will be
;;   buffer-global."
;;   (interactive "P")
;;   (if (save-excursion (org-goto-first-child))
;;       (let ((org-level (org-current-level)))
;;         (avy--generic-jump
;;          (format "^%s "
;;                  (regexp-quote
;;                   (make-string (+ org-level 1) ?*)))
;;          nil
;;          'pre
;;          (unless all
;;            (save-excursion
;;              (ignore-errors
;;                (outline-up-heading 0))
;;              (point)))
;;          (unless all
;;            (save-excursion
;;              (ignore-errors
;;                (outline-up-heading 0))
;;              (org-end-of-subtree)))))
;;     (message "Heading has no children.")))

;; (defun avy-org-goto-level (&optional num)
;;   "Prompt for an org level to go to, defaulting to the current one."
;;   (interactive (list
;;                 (read-number "Select heading level: " (org-current-level))))
;;   (avy--generic-jump
;;    (format "^%s " (regexp-quote (make-string num ?*)))
;;    nil
;;    'pre))

(defun my-buffer-predicate (buffer)
  (if (string-match "helm" (buffer-name buffer))
      nil
    t))
(set-frame-parameter nil 'buffer-predicate 'my-buffer-predicate)


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
