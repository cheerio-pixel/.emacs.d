(provide 'MyMy-mode)
(require 'perspective)
(require 'elpy)
(require 'hideshow)
(require 'org)
(require 'org-roam)
(require 'magit)
(require 'flycheck)
(require 'expand-region)
(use-package ryo-modal
  :ensure t
  :commands ryo-modal-mode
  :bind
  ("S-SPC" . ryo-modal-global-mode)
  ("ü" . ryo-modal-global-mode)
  ("ú" . ryo-modal-global-mode)
  ("ç" . ryo-modal-global-mode)
  ("M-t" . ryo-modal-global-mode)
  :config
  (setq ryo-modal-cursor-color nil)
  (setq ryo-modal-cursor-type 'beam)
  ;; Section-less
  (ryo-modal-keys
   ("/" undo)
   ("oki" helm-M-x)
   ("oks" save-buffer)
   ("p" keyboard-quit)
   ("," ryo-modal-repeat)
   ("m" newline)
   ("M"
    (("o" split-line)
     ("m" smart-open-line)
     )
    )
   ("z"
    (("u" upcase-char)
     ("v" upcase-initials-region)
     ("n" downcase-dwim)
     )
    )
   ("S"
    (("l" org-roam)
     ("f" org-roam-find-file)
     ("F" org-roam-jump-to-index)
     ("i" org-roam-insert)
     ("I" org-roam-insert-immediate)
     ("g" org-roam-graph)
     ("c" org-roam-dailies-capture-today)
     ;; ("r" revert-buffer-no-confirm)
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
   ("i"
    (("y" avy-goto-char)
     ("u" avy-goto-char-2)
     ("m" avy-goto-word-0)
     ("l"pop-global-mark)
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
   (">>" isr/persp-next-buffer)
   ("<<" isr/persp-previous-buffer)
   ("o"
    (("k"
      (("e" helm-mini)
       ("u" kill-buffer)
       ("n" switch-to-last-buffer)
       ("c" kill-current-buffer)
       ("m" helm-find-files :name "Find file")
       ("y" find-name-dired)
       ("r" helm-find :name "Find file recursively") ;; Find files recursively
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
     ("p" ;; Some commands are left
      (("n" persp-next)
       ("u" persp-prev)
       ("y" persp-switch)
       ("m" persp-set-buffer)
       ("k" persp-remove-buffer)
       ("s" projectile-switch-project)
       )
      :name "Projectile & Perspective"
      )
     ("w"
      (("f" ace-window)
       ("s" split-window-below)
       ("r" split-window-right)
       ("d" delete-window)
       ("a" delete-other-windows)
       )
      :name "Windows"
      )
     ("n"
      (("$" mymy/insert-pair$)
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
     )
    )
   )
  ;; Flyckeck
  (ryo-modal-keys
   (:norepeat t)
   ("!"
    (("c" flycheck-buffer)
     ("e" flycheck-explain-error-at-point)
     ("l" flycheck-list-errors)
     ("x" flycheck-disable-checker)
     ("o" flycheck-mode)
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
   ("H"
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
    (("a" org-archive-subtree)
     ("ne" org-narrow-to-element)
     ("nw" widen)
     ;; here
     )
    )
   )
  (ryo-modal-major-mode-keys
   'python-mode
   ;; avalible rstf
   ;; i came up the idea of special rules for the send statment commands
   ;; from the position of the avalible keys, r s t, they will be for the cases
   ;; i feel are the more comfortable and more frequenly used, but this can
   ;; change over time
   ;; (e)(f)(c)(s)(g)(w)(r)(b)
   ("s" ;; elpy interaction
    (("y" ;; eval code section
      ;; text objects & options:
      (("e" ;; send statment
        (("r" elpy-shell-send-statement-and-go)
         ("s" elpy-shell-send-statement)
         ("t" elpy-shell-send-statement-and-step)
         ("f" elpy-shell-send-statement-and-step-and-go)
         )
        )
       ("f" ;; send function
        (("r" elpy-shell-send-defun-and-go)
         ("s" elpy-shell-send-defun)
         ("t" elpy-shell-send-defun-and-step)
         ("f" elpy-shell-send-defun-and-step-and-go)
         )
        )
       ("c" ;; send class
        (("r" elpy-shell-send-defclass-and-go)
         ("s" elpy-shell-send-defclass)
         ("t" elpy-shell-send-defclass-and-step)
         ("f" elpy-shell-send-defclass-and-step-and-go)
         )
        )
       ("s" ;; send top-statment
        (("r" elpy-shell-send-top-statement-and-go)
         ("s" elpy-shell-send-top-statement)
         ("t" elpy-shell-send-top-statement-and-step)
         ("f" elpy-shell-send-top-statement-and-step-and-go)
         )
        )
       ("g" ;; Send Group
        (("r" elpy-shell-send-group-and-go)
         ("s" elpy-shell-send-group)
         ("t" elpy-shell-send-group-and-step)
         ("f" elpy-shell-send-group-and-step-and-go)
         )
        )
       ("w" ;; Send Cell
        ;; A cell it defined by enclosing exprssion with ##
        ;; Like:
        ;; ##
        ;; print("hello world!")
        ;; ##
        (("r" elpy-shell-send-codecell-and-go)
         ("s" elpy-shell-send-codecell)
         ("t" elpy-shell-send-codecell-and-step)
         ("f" elpy-shell-send-codecell-and-step-and-go)
         )
        )
       ("r" ;; Send Region
        (("r" elpy-shell-send-region-or-buffer-and-go)
         ("s" elpy-shell-send-region-or-buffer)
         ("t" elpy-shell-send-region-or-buffer-and-step)
         ("f" elpy-shell-send-region-or-buffer-and-step-and-go)
         )
        )
       ("b" ;; Send Buffer
        (("r" elpy-shell-send-buffer-and-go)
         ("s" elpy-shell-send-buffer)
         ("t" elpy-shell-send-buffer-and-step)
         ("f" elpy-shell-send-buffer-and-step-and-go)
         )
        )
       )
      );; All under this list is in the "y" Section
     ;; Special way of evaluating, there are two like this
     ;; I'm not going to remove the other just to keep things clean
     ;; And because a don't see any harm
     ("RET" elpy-shell-send-statement-and-step)
     ;; Don't know how the specific purpose of this but i think it's to control
     ;; Shortcuts under a protected key, so i don't do <s-any-letter> and shoot
     ;; myself in the feet
     ("c"
      (("w" elpy-black-fix-code)
       ("v" elpy-shell-send-region-or-buffer)
       ("r" elpy-shell-switch-to-shell)
       ("k" elpy-shell-kill)
       ("K" elpy-shell-kill-all) ;; Alt: m
       ("f" elpy-find-file)
       ("s" elpy-rgrep-symbol)
       ("d" elpy-doc)
       ("q" elpy-company-backend)
       ("." elpy-goto-definition-or-rgrep)
       ("," pop-tag-mark)
       )
      )
     ("@" ;; HideShow, Codefolding
      (("c" elpy-folding-toggle-at-point)
       ("b" elpy-folding-toggle-docstrings)
       ("m" elpy-folding-toggle-comments)
       ("f" elpy-folding-hide-leafs)
       ("a" hs-show-all)
       )
      )
     ;; List of busy characters:
     ;; y, c, @
     )
    )
   ;; Testing, Debugging, Refractoring
   ("r"
    (("t" elpy-test) ;; Tests
     ("d" ;; Debugging
      (("d" elpy-pdb-debug-buffer)
       ("b" elpy-pdb-toggle-breakpoint-at-point)
       ("p" elpy-pdb-break-at-point)
       ("e" elpy-pdb-debug-last-exception)
       )
      )
     ("r" ;; Refractoring
      (("e" elpy-multiedit-python-symbol-at-point)
       ("c" elpy-format-code)
       ("r" elpy-refactor-rename)
       ("v" elpy-refactor-extract-variable)
       ("f" elpy-refactor-extract-function)
       ("i" elpy-refactor-inline)
       )
      )
     )
    )
   ("h"
    (("b" er/mark-python-block)
     ("s" er/mark-python-statement)
     ("p" er/mark-python-block-and-decorator)
     ("u" er/mark-outer-python-block)
     ("i" er/mark-inside-python-string)
     ("o" er/mark-outside-python-string)
     )
    )
   ("<f6>" elpy-pdb-toggle-breakpoint-at-point)
   )
  (ryo-modal-key
   "q f" :hydra
   '(hydra-fastmoving ()
                      "Generic fast moving"
                      ("n" forward-sexp)
                      ("u" backward-sexp)
                      ("U" ccm-scroll-down)
                      ("N" ccm-scroll-up)
                      ("]" forward-paragraph)
                      ("[" backward-paragraph)
                      ("q" nil "cancel" :color blue)
                      ))
  (ryo-modal-key
   "q e" :hydra
   '(hydra-elpy ()
                "Elpy Mode"
                ("n" elpy-nav-move-line-or-region-down)
                ("u" elpy-nav-move-line-or-region-up)
                ("N" elpy-nav-indent-shift-left)
                ("U" elpy-nav-indent-shift-right)
                ("h" elpy-nav-forward-block)
                ("l" elpy-nav-backward-block)
                ("H" elpy-nav-backward-indent)
                ("L" elpy-nav-forward-indent)
                ("q" nil "cancel" :color blue)
                ))
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
                  ("n" elpy-nav-move-line-or-region-down)
                  ("u" elpy-nav-move-line-or-region-up)
                  ("N" shift-left)
                  ("U" shift-right)
                  ("q" nil "cancel" :color blue)
                  ))
  (ryo-modal-key
   "d" :hydra
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
                  ("q" nil "cancel" :color blue)
                  ))
  (ryo-modal-key
   "w" :hydra
   '(hydra-undo-tree (:color pink :hint nil)
                     ("n" undo-tree-undo)
                     ("u" undo-tree-redo)
                     ("q" nil "cancel" :color blue)
                     ))
  )
(which-key-add-key-based-replacements "ok" "Buffers, Files & and M-x")
(which-key-add-key-based-replacements "oa" "Insert")
(which-key-add-key-based-replacements "op" "Projectile & Perspective")
(which-key-add-key-based-replacements "ow" "Windows")
(which-key-add-key-based-replacements "ot" "Bookmarks & Registers")
(which-key-add-key-based-replacements "sye" "Send statment")
(which-key-add-key-based-replacements "syf" "Send function")
(which-key-add-key-based-replacements "syc" "Send class")
(which-key-add-key-based-replacements "sys" "Send top-statment")
(which-key-add-key-based-replacements "syg" "Send group")
(which-key-add-key-based-replacements "syw" "Send cell")
(which-key-add-key-based-replacements "syr" "Send region")
(which-key-add-key-based-replacements "syb" "Send buffer")
(which-key-add-key-based-replacements "sy" "Eval code section")
(which-key-add-key-based-replacements "s@" "HideShow")
(which-key-add-key-based-replacements "rd" "Debugging")
(which-key-add-key-based-replacements "rr" "Refractoring")
;;; power-mode.el ends here; I'm still wondering why do they put this
