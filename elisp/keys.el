(require 'centered-cursor-mode)
(require 'dired)
(global-set-key (kbd "C-M-y") 'duplicate-current-line);; Duplicate line
(global-set-key (kbd "C-S-e") 'forward-word)


;;;;;                   UNSET KEY
(global-unset-key (kbd "C-z")) ;; Delete Shortcuts assign
(global-unset-key (kbd "M-z"))
(global-unset-key (kbd "C-q"))
(global-unset-key (kbd "C-t"))

(global-unset-key (kbd "C-v")) ; Imposible to unset without fucking up any other
(global-unset-key (kbd "M-v")) ; thing

(global-unset-key (kbd "C-r")) ;; Here start the rs fb shortcut change
(global-unset-key (kbd "C-s"))
(global-unset-key (kbd "C-b"))
(global-unset-key (kbd "C-f"))
(global-unset-key (kbd "M-f"))
(global-unset-key (kbd "M-b"))
(global-unset-key (kbd "M-s"))
(global-unset-key (kbd "M-r"))
;; (global-unset-key (kbd "M-p"))
(global-unset-key (kbd "M-u"))
(global-unset-key (kbd "C-l"))

(global-unset-key (kbd "C-M-p"))
(global-unset-key (kbd "C-M-f"))
(global-unset-key (kbd "C-M-b"))
(global-unset-key (kbd "C-M-r"))
(global-unset-key (kbd "C-M-s"))
(global-unset-key (kbd "C-M-p"))
(global-unset-key (kbd "C-c C-b"))
(global-unset-key (kbd "C-M-e"))
(global-unset-key (kbd "M-o"))

;;;;;                   set KEY
(global-set-key (kbd "C-t") 'point-to-register)
(global-set-key (kbd "C-S-t") 'jump-to-register)

(global-set-key (kbd "C-o") 'smart-open-line)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
;; (global-set-key (kbd "S-<left>") 'windmove-left)
;; (global-set-key (kbd "S-<up>") 'windmove-up)
;; (global-set-key (kbd "S-<down>") 'windmove-down)
;; (global-set-key (kbd "S-<right>") 'windmove-right)

(global-set-key (kbd "<Scroll_Lock>") 'eval-buffer)
(global-set-key (kbd "M-<Scroll_Lock>") 'eval-region)

(global-set-key (kbd "M-<tab>") 'other-window)

(global-set-key (kbd "C-r") 'backward-char)
(global-set-key (kbd "C-s") 'forward-char)
(global-set-key (kbd "M-r") 'backward-word)
(global-set-key (kbd "M-s") 'forward-to-word)
(global-set-key (kbd "C-M-s") 'forward-sexp)
(global-set-key (kbd "C-M-r") 'backward-sexp)

(global-set-key (kbd "C-M-u") 'backward-list)

(global-set-key (kbd "M-b") 'isearch-backward)
(global-set-key (kbd "M-f") 'isearch-forward)
(global-set-key (kbd "C-M-b") 'isearch-backward-regexp)

(global-set-key (kbd "C-q") 'backward-delete-word)
(global-set-key (kbd "M-z") 'delete-region)
(global-set-key (kbd "M-d") 'delete-word-or-whitespace)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)

(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)
(global-set-key (kbd "C-M-n") 'company-capf)
(global-set-key (kbd "C-M-e") 'company-complete)
(global-set-key (kbd "C-c y y") 'yas-insert-snippet)
;; Rare bindings
;;;; Uninstalled eaf
;; (global-set-key (kbd "C-b") 'switch-to-last-buffer)
;; (global-set-key (kbd "C-f") 'helm-mini)
;; (global-set-key (kbd "C-M-f") 'helm-M-x)
(global-set-key (kbd "M-e") 'hippie-expand)
(global-set-key (kbd "M-n") 'dabbrev-expand)

(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "M-RET") 'dired-find-file)

(define-key centered-cursor-keymap (kbd "C-M--") 'centered-cursor-raise-position-manually)
(define-key centered-cursor-keymap (kbd "C-M-+") 'centered-cursor-lower-position-manually)
(define-key centered-cursor-keymap (kbd "C-M-=") 'centered-cursor-lower-position-manually)
(define-key centered-cursor-keymap (kbd "C-M-0") 'centered-cursor-reset-position-manually)

(global-unset-key (kbd "C-d"))
(global-set-key (kbd "C-c s u") 'straight-use-package)
(global-set-key (kbd "C-c s g") 'straight-get-recipe)

;; (global-set-key (kbd "<f5>") 'keyboard-escape-quit) This is not the way to bind a command of this kind

;; It provokes Meta to behave like C-g in the terminal
;; (define-key key-translation-map (kbd "ESC") (kbd "C-g"))
(define-key key-translation-map (kbd "C-p") (kbd "C-u"))
(define-key key-translation-map (kbd "C-u") (kbd "C-p"))

;; (define-key key-translation-map (kbd "") (kbd ""))



;; Don't know why it doesn't work
;; (define-key dired-mode-map (kbd "F")
;;   (lambda ()
;;     (interactive)
;;     (mapc #'find-file (reverse (dired-get-marked-files)))))
(provide 'keys)
