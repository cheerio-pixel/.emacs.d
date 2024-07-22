
(setq dropbox-dir
      (pcase system-type
        ('windows-nt "c:/Users/frail/Dropbox/")
        ('gnu/linux "~/Dropbox (Maestral)/")))

(setq main-dropbox-dir (concat dropbox-dir "Creativè/"))

(setq mymy-org-roam-dir (concat main-dropbox-dir "Notes/"))

(defvar mymy-organization-system-directory (concat dropbox-dir "org/")
  "General purpose root directory of notes")

;; Check
(unless (file-exists-p mymy-organization-system-directory)
  (error "Cannot find '%s'. Directory doesn't exist " mymy-organization-system-directory))

(defvar mymy-organization-system-directory-attachments
  (concat mymy-organization-system-directory "attachments/")
  "Attachment directory")

(defvar mymy-bibliography-system-directory
  (concat mymy-organization-system-directory "bibliography_system/")
  "Diretory of bibliography references.")

(unless (file-exists-p mymy-bibliography-system-directory)
  (error "Cannot find '%s'. Directory doesn't exist " mymy-bibliography-system-directory))

(when (and (file-exists-p mymy-organization-system-directory)
           (not (file-exists-p mymy-organization-system-directory-attachments)))
  (make-directory mymy-organization-system-directory-attachments))

(provide 'vars.el)