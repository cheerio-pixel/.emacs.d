(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;;* Straight config
(setq straight-use-package-by-default t)
(setq straight-host-usernames '((github . "cheerio-pixel")))

(straight-use-package 'use-package)
(eval-when-compile (require 'use-package))
;; ---------------------------------------------------------------------------------------------

