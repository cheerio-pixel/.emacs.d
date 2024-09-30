;;; spec-keywords.el --- Highlight Spec Keywords  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Frairlyn Camilo Roque Suarez

;; Author: Frairlyn Camilo Roque Suarez <frailin300@gmail.com>
;; Homepage: https://github.com/yourusername/spec-keywords
;; Keywords: convenience

;; Package-Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a minor mode for highlighting Spec Keywords
;; in Emacs buffers.  It allows customization of keywords, faces,
;; and provides navigation commands.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(defgroup spec-keywords nil
  "Highlight Spec Keywords in buffers."
  :group 'font-lock-extra-types)

(defface spec-keywords-base-face
  '((t (:bold t :foreground "forest green")))
  "Base face used to highlight Spec Keywords."
  :group 'spec-keywords)

(defcustom spec-keywords-keyword-faces
  '(("REPO"        . "royal blue")
    ("USER STEP"   . "dark orchid")
    ("USING SPEC"  . "dark orange")
    ("USING"       . "orange")
    ("USING SYSTEM" . "sienna")
    ("USING ACTION" . "purple")
    ("DEPENDS ON"  . "dark red")
    ("RUN"         . "forest green")
    ("ENV"         . "dark cyan")
    ("COPY"        . "dark goldenrod")
    ("MV"          . "dark goldenrod")
    ("EXAMPLE"     . "medium blue")
    ("ACTION"      . "red")
    ("GUI GOTO"    . "dark magenta")
    ("THEN"        . "magenta")
    ("CLICK"       . "slate blue")
    ("USING TEXT"  . "dark green")
    ("PASTE INTO"  . "dark olive green")
    ("COPY TEXT"   . "dark olive green")
    ("UNCOMMENT"   . "firebrick")
    ("COMMENT"     . "firebrick")
    ("TO"          . "indian red"))
  "An alist mapping keywords to colors used to display them."
  :group 'spec-keywords
  :type '(repeat (cons (string :tag "Keyword")
                       (choice :tag "Face   "
                               (string :tag "Color")
                               (sexp :tag "Face")))))

(defcustom spec-keywords-include-modes '(prog-mode text-mode)
  "Major modes in which `spec-keywords-mode' should be activated."
  :group 'spec-keywords
  :type '(repeat function))

(defcustom spec-keywords-exclude-modes '()
  "Major modes in which `spec-keywords-mode' should not be activated."
  :group 'spec-keywords
  :type '(repeat function))

(defcustom spec-keywords-highlight-punctuation ""
  "String of characters to highlight after keywords."
  :group 'spec-keywords
  :type 'string)

(defvar spec-keywords--keywords
  `((,(lambda (bound) (spec-keywords--search nil bound))
     (1 (spec-keywords--get-face) prepend t))))

(defvar-local spec-keywords--regexp nil)

(defun spec-keywords--regexp ()
  "Return regular expression matching Spec Keywords."
  (or spec-keywords--regexp (spec-keywords--setup-regexp)))

(defun spec-keywords--setup-regexp ()
  "Setup keyword regular expression."
  (setq spec-keywords--regexp
        (concat "\\(\\<"
                "\\(" (mapconcat #'car spec-keywords-keyword-faces "\\|") "\\)"
                "\\>"
                (and (not (equal spec-keywords-highlight-punctuation ""))
                     (concat "[" spec-keywords-highlight-punctuation "]*"))
                "\\)")))

(defvar spec-keywords--syntax-table (copy-syntax-table text-mode-syntax-table))

(defun spec-keywords--search (&optional regexp bound backward)
  "Search for keyword REGEXP, optionally up to BOUND and BACKWARD."
  (unless regexp
    (setq regexp (spec-keywords--regexp)))
  (let ((case-fold-search nil))
    (funcall (if backward #'re-search-backward #'re-search-forward)
             regexp bound t)))

(defun spec-keywords--get-face ()
  "Return face for current keyword during font locking."
  (let* ((keyword (match-string 2))
         (color (cdr (assoc keyword spec-keywords-keyword-faces))))
    (spec-keywords--combine-face color)))

(defun spec-keywords--combine-face (color)
  "Combine COLOR with `spec-keywords-base-face'."
  (if (stringp color)
      `((:foreground ,color) spec-keywords-base-face)
    color))

(defvar-keymap spec-keywords-mode-map
  :doc "Keymap for `spec-keywords-mode'.")

;;;###autoload
(define-minor-mode spec-keywords-mode
  "Highlight Spec Keywords in comments and strings."
  :lighter " SpecKW"
  :keymap spec-keywords-mode-map
  :group 'spec-keywords
  (if spec-keywords-mode
      (font-lock-add-keywords nil spec-keywords--keywords t)
    (font-lock-remove-keywords nil spec-keywords--keywords))
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer)))))

;;;###autoload
(define-globalized-minor-mode global-spec-keywords-mode
  spec-keywords-mode
  (lambda ()
    (when (and (not (minibufferp))
               (apply #'derived-mode-p spec-keywords-include-modes)
               (not (apply #'derived-mode-p spec-keywords-exclude-modes)))
      (spec-keywords-mode 1))))

(provide 'spec-keywords)

;;; spec-keywords.el ends here