(setq mymy-we-are-at-work
      (getenv "WE_ARE_AT_WORK"))
(setq mymy-we-are-not-at-work (null mymy-we-are-at-work))

(setq dropbox-dir
      (pcase system-type
        ('windows-nt "c:/Users/frail/Dropbox/")
        ('gnu/linux "~/Dropbox (Maestral)/")))

(setq main-dropbox-dir (concat dropbox-dir "Creativè/"))

(setq mymy-org-roam-dir (concat main-dropbox-dir "Notes/"))

(defconst mymy-organization-system-directory (concat dropbox-dir "org/")
  "General purpose root directory of notes")

(defconst mymy-organization-system-directory-text
  (expand-file-name
   "text"
   mymy-organization-system-directory)
  "The directory of the text files.")

;; Check

(when mymy-we-are-not-at-work
  (unless (file-exists-p mymy-organization-system-directory)
    (error "Cannot find '%s'. Directory doesn't exist " mymy-organization-system-directory))
  )

(defconst mymy-organization-system-directory-attachments
  (concat mymy-organization-system-directory "attachments/")
  "Attachment directory")

(defconst mymy-bibliography-system-directory
  (expand-file-name 
   "bibliography_system" 
   mymy-organization-system-directory-text)
  "Diretory of bibliography references.")


(when mymy-we-are-not-at-work
  (unless (file-exists-p mymy-bibliography-system-directory)
    (error "Cannot find '%s'. Directory doesn't exist " mymy-bibliography-system-directory))
  (when (and (file-exists-p mymy-organization-system-directory)
             (not (file-exists-p mymy-organization-system-directory-attachments)))
    (make-directory mymy-organization-system-directory-attachments))
  )


(provide 'vars.el)
