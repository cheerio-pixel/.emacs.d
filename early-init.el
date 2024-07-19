(setq package-enable-at-startup nil)
(setq source-directory "~/.local/src/emacs/")
(defvar native-comp-deferred-compilation-deny-list nil)
(defalias 'redraw-modeline 'force-mode-line-update)
(defvar lsp-use-plists t)
(setenv "LSP_USE_PLISTS" "true")

;; ;; https://github.com/bkaestner/.emacs.d/blob/37wc75bfe3a199594ad89504d870e68f6f424764f/early-init.el
;; (setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
;;       gc-cons-percentage 0.6)
;; ;; After Emacs has completely started, reset the values to more sensible ones.
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (setq gc-cons-threshold 100000000
;;                   gc-cons-percentage 0.1)))
;; (setenv "LSP_USE_PLISTS" "true")
