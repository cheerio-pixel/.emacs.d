(setq package-enable-at-startup nil)

;; https://github.com/bkaestner/.emacs.d/blob/37c75bfe3a199594ad89504d870e68f6f424764f/early-init.el
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)
;; After Emacs has completely started, reset the values to more sensible ones.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 100000000
                  gc-cons-percentage 0.1)))
