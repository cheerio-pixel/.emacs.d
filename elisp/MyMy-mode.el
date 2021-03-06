;; (require 'elpy)
(require 'org)
(require 'hideshow)
(require 'org-roam)
(require 'magit)
(require 'flycheck)
(require 'expand-region)
(require 'yasnippet)
(require 'csv-mode)
(require 'centaur-tabs)

(defun export-subtree-to-pdf()
  (interactive)
  (org-latex-export-to-pdf nil t nil nil nil)
  )
(defun tmp-lispy-fix ()
  (interactive)
  (if ryo-modal-global-mode
      (progn
        (ryo-modal-global-mode 0)
        (lispy-mode 1))
    (ryo-modal-global-mode 1)
    (lispy-mode -1)
    )
  )
(defun toggle-centaur-grouping ()
  (interactive)
  (if (eq centaur-tabs-buffer-groups-function 'centaur-tabs-projectile-buffer-groups)
      (centaur-tabs-group-buffer-groups)
    (centaur-tabs-group-by-projectile-project)
    )
  )
(use-package ryo-modal
  :ensure t
  :commands ryo-modal-mode
  :bind
  ("S-SPC" . ryo-modal-global-mode)
  ("ç" . ryo-modal-global-mode)
  ("C-t" . ryo-modal-global-mode)
  :config
  (setq ryo-modal-cursor-color nil)
  (setq ryo-modal-cursor-type 'beam)
  ;; Section-less
  (ryo-modal-keys
   ("C-\\" toggle-input-method :exit 1)
   ("gr" golden-ratio-mode)
   ("gd" remember)
   ("gD" remember-notes)
   ("gy" yas-visit-snippet-file)
   ("Gy" yas-insert-snippet)
   ("gi" quoted-insert)
   ("/" undo)
   ("p" keyboard-quit)
   ("," ryo-modal-repeat)
   ("m" newline)
   ("M"
    (("o" split-line)
     ("m" smart-open-line)
     ("n" open-line)
     )
    )
   ("z"
    (("u" upcase-char)
     ("U" upcase-region)
     ("v" upcase-initials-region)
     ("n" downcase-dwim)
     )
    )
   ("S"
    (("l" org-roam-buffer-toggle)
     ("f" org-roam-node-find)
     ("i" org-roam-node-insert)
     ("I" org-roam-node-insert-immediate)
     ("g" org-roam-graph)
     ("t" org-roam-tag-add)
     ("c" org-roam-dailies-capture-today)
     ("o" org-id-get-create)
     ("p" org-toggle-properties)
     )
    )
   ("Z"
    (("v" fill-region)
     ("s" my-save-word)
     )
    )
   ("Gg" magit-status)
   ;; r, s, R, S are reserved for major modes
   )
  ;;; Motion
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
   ("E" jump-to-register)
   ("ga" beginning-of-buffer)
   ("ge" end-of-buffer)
   ("gu" scroll-down)
   ("gn" scroll-up)
   )
  (ryo-modal-keys ;;; Avy
   ("I" avy-goto-word-1)
   ("i"
    (("y" avy-goto-char)
     ("u" avy-goto-char-2)
     ("m" avy-goto-word-0)
     ("l" helm-all-mark-rings)
     ("k" avy-goto-word-1)
     ("n" avy-goto-word-crt-line)
     ("e" avy-goto-word-forward-crt-line)
     ("i" avy-goto-line)
     ("/"
      (("m" avy-goto-word-0-below)
       ("k" avy-goto-word-1-below)
       )
      )
     (";"
      (("m" avy-goto-word-0-above)
       ("k" avy-goto-word-1-above)
       )
      )
     ("a" ace-mc-add-multiple-cursors)
     ("t" ace-mc-add-single-cursor)
     )
    )
   )
  (ryo-modal-keys ;;; Registers
   ("c" forward-to-word)
   ("C" forward-word)
   ("X" backward-to-word)
   ("x" backward-word)
   ("o"
    (("tt" helm-register)
     ("tp" helm-bookmarks)
     ("ts" bookmark-set)
     ("td" bookmark-delete)
     )
    )
   )
  ;; delete
  (ryo-modal-keys
   ("D"
    (("t" my-delete-whole-line)
     ("d" backward-delete-word)
     ("v" delete-region)
     ("f" delete-word)
     ("s" delete-char)
     ("r" backward-delete-char)
     ("h" delete-line)
     ("(" delete-pair)
     ("SPC" delete-horizontal-space)
     ("/" delete-blank-lines)
     )
    )
   )
  ;; kill
  (ryo-modal-keys
   ("k" kill-line)
   ("K"
    (("a" kill-whole-line)
     ("v" kill-region)
     ("w" kill-ring-save)
     )
    )
   )
  ;; yank
  (ryo-modal-keys
   ("y" yank)
   ("Y"
    (("an" duplicate-current-line)
     ("nd" replace-next-line)
     ("ad" replace-current-line)
     ("u" helm-show-kill-ring)
     ("m" paste-primary-selection)
     ("M" yank-at-point)
     )
    )
   )
  ;; search
  (ryo-modal-keys
   ("f" isearch-forward)
   ("b" isearch-backward)
   ("F" helm-swoop)
   ("B" helm-semantic-or-imenu)
   )
  ;; Buffers
  (ryo-modal-keys
   (">>" centaur-tabs-move-current-tab-to-right)
   ("<<" centaur-tabs-move-current-tab-to-left)
   ("O"
    (("k"
      (("m" direx:jump-to-directory)
       )
      )
     )
    )
   ("o"
    (("l" helm-locate)
     ("k"
      (("e" helm-mini)
       ("u" kill-buffer)
       ("n" switch-to-last-buffer)
       ("c" kill-current-buffer)
       ("x" reopen-killed-file)
       ("X" reopen-killed-file-fancy)
       ("m" helm-find-files :name "Find file")
       ("y" find-name-dired)
       ("r" helm-find :name "Find file recursively") ;; Find files recursively
       ("i" helm-M-x)
       ("s" save-buffer)
       ("o" helm-apropos)
       ("l" create-scratch-buffer)
       )
      :name "Buffers, Files & and M-x"
      )
     ("a"
      (("t" ignore :read t :name "insert text")
       ("p" insert-parentheses)
       ("q" insert-quotes)
       ("c" comment-dwim :name "Comment")
       ("i" string-rectangle)
       )
      :name "Insert"
      )
     ("w"
      (("f" ace-window)
       ("r" split-window-below)
       ("s" split-window-right)
       ("d" delete-window)
       ("a" delete-other-windows)
       ("t" make-frame)
       ("p" delete-frame)
       )
      :name "Windows"
      )
     ("n"
      (("$" mymy/insert-pair$)
       ("~" mymy/insert-pair~)
       ("=" mymy/insert-pair=)
       ("\\" insert-pair)
       )
      :name "Insert pairs"
      )
     )
    )
   )
  ;; Org mode
  (ryo-modal-keys
   (:norepeat t)
   ("O"
    (("c" org-capture)
     ("a" org-agenda)
     ("t" org-todo-list)
     ("u" org-agenda-clock-goto)
     ("y" export-subtree-to-pdf)
     ("m"
      (("y" org-goto-tasks)
       ("n" org-goto-school-schedule)
       )
      )
     )
    )
   )
  ;; Flyckeck
  (ryo-modal-keys
   (:norepeat t)
   ("!"
    (("c" flycheck-buffer :name "Check buffer")
     ("e" flycheck-explain-error-at-point :name "Explain error at point")
     ("l" flycheck-list-errors :name "List errors")
     ("x" flycheck-disable-checker :name "Disable checker")
     ("o" flycheck-mode :name "Toggle mode")
     )
    )
   )
  ;; selection
  (ryo-modal-keys
   (:norepeat t)
   ("v" set-mark-command)
   ("V"
    (("v" rectangle-mark-mode)
     ("c" exchange-point-and-mark)
     )
    )
   ("\'" er/expand-region)
   ("On"
    (("c" er/mark-comment)
     ("d" er/mark-defun)
     ("w" er/mark-word)
     ("m" er/mark-method-call)
     ("s" er/mark-symbol)
     ("a" er/mark-symbol-with-prefix)
     ("q"
      (("i" er/mark-inside-quotes)
       ("o" er/mark-outside-quotes)
       )
      )
     ("p"
      (("i" er/mark-inside-pairs)
       ("o" er/mark-outside-pairs)
       )
      )
     ("n" er/mark-next-accessor) ;; skips over one period and mark next symbol
     )
    )
   )
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
   ("9" "M-9")
   )
  (defun cider-find-and-clear-repl-buffer ()
    (interactive)
    (cider-find-and-clear-repl-output t)
    )
  (defun cider-pprint-eval-defun-at-point-to-comment ()
    (interactive)
    (cider-pprint-eval-defun-at-point t))
  (ryo-modal-major-mode-keys
   'clojure-mode
   ("S-SPC" tmp-lispy-fix)
   ("C-t" tmp-lispy-fix)
   ("r"
    (("r" cider-run)
     ("f" cider-pprint-eval-defun-at-point)
     ("c" cider-find-and-clear-repl-output)
     ("C" cider-find-and-clear-repl-buffer)
     ("p" cider-eval-defun-to-comment)
     ("P" cider-pprint-eval-defun-at-point-to-comment)
     )
    )
   ("M-RET" outline-insert-heading)
   ("M-N" outline-promote)
   ("M-U" outline-demote)
   ("M-e" outline-toggle-children)
   )
  (ryo-modal-major-mode-keys
   'csv-mode
   ("s"
    (("t" csv-align-fields)
     )
    )
   )
  ;; emacs-lisp mode map
  (ryo-modal-major-mode-keys
   'emacs-lisp-mode
   ("s"
    (("s" eval-buffer)
     ("r" eval-region)
     ("f" eval-defun)
     )
    )
   )
  (ryo-modal-major-mode-keys
   'org-mode
   ;; avalible r s
   ("M-m" org-meta-return)
   ("M-c" org-toggle-checkbox)
   ("C-M-m" org-insert-heading-respect-content)
   ("M-U" org-metaright)
   ("M-N" org-metaleft)
   ("m" org-return)
   ("r"
    (("s" org-preview-latex-fragment)
     ("t" org-latex-preview)
     ("l" org-toggle-link-display)
     ("o" org-open-at-point)
     ("i" +org-toggle-inline-image-at-point)
     )
    )
   ("s"
    ;; here
    (;; ("a" org-agenda-archive-subtree)
     ("A" org-agenda-toggle-archive-tag)
     ("p" er/mark-org-element)
     ("t" outline-up-heading)
     ("T" er/mark-org-parent)
     ("g" org-agenda-goto)
     ("ne" org-narrow-to-element)
     ("nv" narrow-to-region)
     ("nw" widen)
     ("m" outline-show-children)
     ("k" outline-show-branches)
     ;; here
     )
    )
   ("Y"
    (("y" org-paste-special)
     )
    )
   ("V"
    (("b" org-mark-subtree)
     )
    )
   ("K"
    (("V" org-cut-special)
     ("W" org-copy-special)
     )
    )
   ("a" org-beginning-of-line)
   ("e" org-end-of-line)
   ("O"
    (("o" org-agenda-clock-out)
     ("i" org-agenda-clock-in)
     ("r" org-agenda-refile)
     ("e" org-agenda-clock-cancel)
     ("d" org-insert-drawer)
     )
    )
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
                       ("q" nil "cancel" :color blue)
                       )
    )
   ("q n" :hydra
    '(hydra-org-heading-move ()
                             "Heading move Mode"
                             ("n" org-forward-heading-same-level)
                             ("u" org-backward-heading-same-level)
                             ("U" org-metaup)
                             ("N" org-metadown)
                             ("q" nil "cancel" :color blue)
                             ))
   )
  (ryo-modal-major-mode-keys
   'python-mode
   ;; avalible rstf
   ;; i came up the idea of special rules for the send statment commands
   ;; from the position of the avalible keys, r s t, they will be for the cases
   ;; i feel are the more comfortable and more frequenly used, but this can
   ;; change over time
   ;; (e)(f)(c)(s)(g)(w)(r)(b)
   ;; ("s" ;; elpy interaction
   ;;  (("y" ;; eval code section
   ;;    ;; text objects & options:
   ;;    (("e" ;; send statment
   ;;      (("r" elpy-shell-send-statement-and-go)
   ;;       ("s" elpy-shell-send-statement)
   ;;       ("t" elpy-shell-send-statement-and-step)
   ;;       ("f" elpy-shell-send-statement-and-step-and-go)
   ;;       )
   ;;      )
   ;;     ("f" ;; send function
   ;;      (("r" elpy-shell-send-defun-and-go)
   ;;       ("s" elpy-shell-send-defun)
   ;;       ("t" elpy-shell-send-defun-and-step)
   ;;       ("f" elpy-shell-send-defun-and-step-and-go)
   ;;       )
   ;;      )
   ;;     ("c" ;; send class
   ;;      (("r" elpy-shell-send-defclass-and-go)
   ;;       ("s" elpy-shell-send-defclass)
   ;;       ("t" elpy-shell-send-defclass-and-step)
   ;;       ("f" elpy-shell-send-defclass-and-step-and-go)
   ;;       )
   ;;      )
   ;;     ("s" ;; send top-statment
   ;;      (("r" elpy-shell-send-top-statement-and-go)
   ;;       ("s" elpy-shell-send-top-statement)
   ;;       ("t" elpy-shell-send-top-statement-and-step)
   ;;       ("f" elpy-shell-send-top-statement-and-step-and-go)
   ;;       )
   ;;      )
   ;;     ("g" ;; Send Group
   ;;      (("r" elpy-shell-send-group-and-go)
   ;;       ("s" elpy-shell-send-group)
   ;;       ("t" elpy-shell-send-group-and-step)
   ;;       ("f" elpy-shell-send-group-and-step-and-go)
   ;;       )
   ;;      )
   ;;     ("w" ;; Send Cell
   ;;      ;; A cell it defined by enclosing exprssion with ##
   ;;      ;; Like:
   ;;      ;; ##
   ;;      ;; print("hello world!")
   ;;      ;; ##
   ;;      (("r" elpy-shell-send-codecell-and-go)
   ;;       ("s" elpy-shell-send-codecell)
   ;;       ("t" elpy-shell-send-codecell-and-step)
   ;;       ("f" elpy-shell-send-codecell-and-step-and-go)
   ;;       )
   ;;      )
   ;;     ("r" ;; Send Region
   ;;      (("r" elpy-shell-send-region-or-buffer-and-go)
   ;;       ("s" elpy-shell-send-region-or-buffer)
   ;;       ("t" elpy-shell-send-region-or-buffer-and-step)
   ;;       ("f" elpy-shell-send-region-or-buffer-and-step-and-go)
   ;;       )
   ;;      )
   ;;     ("b" ;; Send Buffer
   ;;      (("r" elpy-shell-send-buffer-and-go)
   ;;       ("s" elpy-shell-send-buffer)
   ;;       ("t" elpy-shell-send-buffer-and-step)
   ;;       ("f" elpy-shell-send-buffer-and-step-and-go)
   ;;       )
   ;;      )
   ;;     )
   ;;    );; All under this list is in the "y" Section
   ;;   ;; Special way of evaluating, there are two like this
   ;;   ;; I'm not going to remove the other just to keep things clean
   ;;   ;; And because a don't see any harm
   ;;   ("RET" elpy-shell-send-statement-and-step)
   ;;   ("b" blacken-buffer)
   ;;   ;; Don't know how the specific purpose of this but i think it's to control
   ;;   ;; Shortcuts under a protected key, so i don't do <s-any-letter> and shoot
   ;;   ;; myself in the feet
   ;;   ("c"
   ;;    (("w" elpy-black-fix-code)
   ;;     ("v" elpy-shell-send-region-or-buffer)
   ;;     ("r" elpy-shell-switch-to-shell)
   ;;     ("k" elpy-shell-kill)
   ;;     ("K" elpy-shell-kill-all) ;; Alt: m
   ;;     ("f" elpy-find-file)
   ;;     ("s" elpy-rgrep-symbol)
   ;;     ("d" elpy-doc)
   ;;     ("q" elpy-company-backend)
   ;;     ("." elpy-goto-definition-or-rgrep)
   ;;     ("," pop-tag-mark)
   ;;     )
   ;;    )
   ;;   ("@" ;; HideShow, Codefolding
   ;;    (("c" elpy-folding-toggle-at-point)
   ;;     ("b" elpy-folding-toggle-docstrings)
   ;;     ("m" elpy-folding-toggle-comments)
   ;;     ("f" elpy-folding-hide-leafs)
   ;;     ("a" hs-show-all)
   ;;     )
   ;;    )
   ;;   ;; List of busy characters:
   ;;   ;; y, c, @
   ;;   )
   ;;  )
   ;; Testing, Debugging, Refractoring
   ;; ("r"
   ;;  (("t" elpy-test) ;; Tests
   ;;   ("d" ;; Debugging
   ;;    (("d" elpy-pdb-debug-buffer)
   ;;     ("b" elpy-pdb-toggle-breakpoint-at-point)
   ;;     ("p" elpy-pdb-break-at-point)
   ;;     ("e" elpy-pdb-debug-last-exception)
   ;;     )
   ;;    )
   ;;   ("r" ;; Refractoring
   ;;    (("e" elpy-multiedit-python-symbol-at-point)
   ;;     ("c" elpy-format-code)
   ;;     ("r" elpy-refactor-rename)
   ;;     ("v" elpy-refactor-extract-variable)
   ;;     ("f" elpy-refactor-extract-function)
   ;;     ("i" elpy-refactor-inline)
   ;;     )
   ;;    )
   ;;   )
   ;;  )
   ("h"
    (("b" er/mark-python-block)
     ("s" er/mark-python-statement)
     ("p" er/mark-python-block-and-decorator)
     ("u" er/mark-outer-python-block)
     ("i" er/mark-inside-python-string)
     ("o" er/mark-outside-python-string)
     )
    )
   ;; ("<f6>" elpy-pdb-toggle-breakpoint-at-point)
   )
  (ryo-modal-key
   "q f" :hydra
   '(hydra-fastmoving ()
                      "Generic fast moving"
                      ("n" scroll-up)
                      ("u" scroll-down)
                      ("U" ccm-scroll-down)
                      ("N" ccm-scroll-up)
                      ("]" forward-paragraph)
                      ("[" backward-paragraph)
                      ("q" nil "cancel" :color blue)
                      ))
  ;; (ryo-modal-key
  ;;  "q e" :hydra
  ;;  '(hydra-elpy ()
  ;;               "Elpy Mode"
  ;;               ("n" elpy-nav-move-line-or-region-down)
  ;;               ("u" elpy-nav-move-line-or-region-up)
  ;;               ("N" elpy-nav-indent-shift-left)
  ;;               ("U" elpy-nav-indent-shift-right)
  ;;               ("h" elpy-nav-forward-block)
  ;;               ("l" elpy-nav-backward-block)
  ;;               ("H" elpy-nav-backward-indent)
  ;;               ("L" elpy-nav-forward-indent)
  ;;               ("q" nil "cancel" :color blue)
  ;;               ))
  (ryo-modal-key
   "q ; f" :hydra
   '(hydra-syntaxcheck ()
                       "SyntaxCheck Mode"
                       ("n" flycheck-next-error :name "Next error")
                       ("u" flycheck-previous-error :name "Previous error")
                       ("ev" flycheck-buffer)
                       ("q" nil "cancel" :color blue)
                       ))
  (ryo-modal-key
   "q i" :hydra
   '(hydra-indent ()
                  "Indent Mode"
                  ("n" mymy/elpy-nav-move-line-or-region-down)
                  ("u" mymy/elpy-nav-move-line-or-region-up)
                  ("N" shift-left)
                  ("U" shift-right)
                  ("q" nil "cancel" :color blue)
                  ))
  (ryo-modal-key
   "q d" :hydra
   '(hydra-delete (
                   :color pink
                   :hint nil)
                  "
               ^Delete^
^^----------------------------------------
_t_: whole-line    _r_: unmark
_d_: backward-word _(_: unmark up
_v_: delete        _SPC_: horizontal-space
_f_: delete up     _/_: blank-lines
_s_: modified      ^ ^
"
                  ("t" my-delete-whole-line)
                  ("d" backward-delete-word)
                  ("v" delete-region)
                  ("f" delete-word)
                  ("s" delete-char)
                  ("r" backward-delete-char)
                  ("(" delete-pair)
                  ("SPC" delete-horizontal-space)
                  ("/" delete-blank-lines)
                  ("h" delete-line)
                  ;; Motion
                  ("n" next-line)
                  ("u" previous-line)
                  ("N" backward-char)
                  ("U" forward-char)
                  ("M-n" forward-sentence)
                  ("M-u" backward-sentence)
                  ("a" smarter-move-beginning-of-line)
                  ("e" end-of-line)
                  ("A" point-to-register)
                  ("E" jump-to-register)
                  ("ga" beginning-of-buffer)
                  ("ge" end-of-buffer)
                  ("q" nil "cancel" :color blue))))
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
  "rr" "Refractoring"
  )
(provide 'MyMy-mode)
;;; MyMy-mode.el ends here
