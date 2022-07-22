;; -*- lexical-binding: t; -*-
(require 'org-roam)
(require 'el-patch)
(require 'cl-lib)

;; Here begins all that i have done to have backlink-counts
(el-patch-defconst org-roam-db--table-schemata
  '((files
     [(file :unique :primary-key)
      title
      (hash :not-null)
      (atime :not-null)
      (mtime :not-null)])

    (nodes
     ([(id :not-null :primary-key)
       (file :not-null)
       (level :not-null)
       (pos :not-null)
       todo
       priority
       (scheduled text)
       (deadline text)
       title
       properties
       olp
       (el-patch-add backlinkcount)
       ]
      (:foreign-key [file] :references files [file] :on-delete :cascade)))
    (aliases
     ([(node-id :not-null)
       alias]
      (:foreign-key [node-id] :references nodes [id] :on-delete :cascade)))

    (citations
     ([(node-id :not-null)
       (cite-key :not-null)
       (pos :not-null)
       properties]
      (:foreign-key [node-id] :references nodes [id] :on-delete :cascade)))

    (refs
     ([(node-id :not-null)
       (ref :not-null)
       (type :not-null)]
      (:foreign-key [node-id] :references nodes [id] :on-delete :cascade)))

    (tags
     ([(node-id :not-null)
       tag]
      (:foreign-key [node-id] :references nodes [id] :on-delete :cascade)))

    (links
     ([(pos :not-null)
       (source :not-null)
       (dest :not-null)
       (type :not-null)
       (properties :not-null)]
      (:foreign-key [source] :references nodes [id] :on-delete :cascade)))))

(cl-defstruct (org-roam-node (:constructor org-roam-node-create)
                             (:copier nil))
  "A heading or top level file with an assigned ID property."
  file file-title file-hash file-atime file-mtime
  id level point todo priority scheduled deadline title properties olp
  tags aliases refs backlink-count)

(el-patch-cl-defun org-roam-db-insert-node-data ()
  "Insert node data for headline at point into the Org-roam cache."
  (when-let ((id (org-id-get)))
    (let* ((file (buffer-file-name (buffer-base-buffer)))
           (heading-components (org-heading-components))
           (pos (point))
           (todo (nth 2 heading-components))
           (priority (nth 3 heading-components))
           (level (nth 1 heading-components))
           (scheduled (org-roam-db-get-scheduled-time))
           (deadline (org-roam-db-get-deadline-time))
           (title (or (nth 4 heading-components)
                      (progn (lwarn 'org-roam :warning "Node in %s:%s:%s has no title, skipping..."
                                    file
                                    (line-number-at-pos)
                                    (1+ (- (point) (line-beginning-position))))
                             (cl-return-from org-roam-db-insert-node-data))))
           (properties (org-entry-properties))
           (olp (org-get-outline-path nil 'use-cache))
           (title (org-link-display-format title))
           (el-patch-add (backlink (caar (org-roam-db-query
                                          [:select (funcall count source)
                                                   :from links
                                                   :where (= dest $s1)
                                                   :and (= type "id")
                                                   ] id)))))
      (org-roam-db-query!
       (lambda (err)
         (lwarn 'org-roam :warning "%s for %s (%s) in %s"
                (error-message-string err)
                title id file))
       [:insert :into nodes
                :values $v1]
       (vector id file level pos todo priority
               scheduled deadline title properties olp (el-patch-add backlink))))))
(el-patch-defun org-roam-db-insert-file-node ()
  "Insert the file-level node into the Org-roam cache."
  (org-with-point-at 1
    (when (and (= (org-outline-level) 0)
               (org-roam-db-node-p))
      (when-let ((id (org-id-get)))
        (let* ((file (buffer-file-name (buffer-base-buffer)))
               (title (org-roam-db--file-title))
               (pos (point))
               (todo nil)
               (priority nil)
               (scheduled nil)
               (deadline nil)
               (level 0)
               (tags org-file-tags)
               (properties (org-entry-properties))
               (olp nil)
               (el-patch-add (backlink (caar (org-roam-db-query
                                              [:select (funcall count source)
                                                       :from links
                                                       :where (= dest $s1)
                                                       :and (= type "id")
                                                       ] id)))))
          (org-roam-db-query!
           (lambda (err)
             (lwarn 'org-roam :warning "%s for %s (%s) in %s"
                    (error-message-string err)
                    title id file))
           [:insert :into nodes
                    :values $v1]
           (vector id file level pos todo priority
                   scheduled deadline title properties olp (el-patch-add backlink)))
          (when tags
            (org-roam-db-query
             [:insert :into tags
                      :values $v1]
             (mapcar (lambda (tag)
                       (vector id (substring-no-properties tag)))
                     tags)))
          (org-roam-db-insert-aliases)
          (org-roam-db-insert-refs))))))
(el-patch-defun org-roam-node-list ()
  "Return all nodes stored in the database as a list of `org-roam-node's."
  (let ((rows (org-roam-db-query
               "SELECT
  id,
  file,
  filetitle,
  \"level\",
  todo,
  pos,
  priority ,
  scheduled ,
  deadline ,
  title,
  properties ,
  olp,
  atime,
  mtime,
  '(' || group_concat(tags, ' ') || ')' as tags,
  aliases,
  refs,
  backlinkcount
FROM
  (
  SELECT
    id,
    file,
    filetitle,
    \"level\",
    todo,
    pos,
    priority ,
    scheduled ,
    deadline ,
    title,
    properties ,
    olp,
    atime,
    mtime,
    tags,
    '(' || group_concat(aliases, ' ') || ')' as aliases,
    refs,
    backlinkcount
  FROM
    (
    SELECT
      nodes.id as id,
      nodes.file as file,
      nodes.\"level\" as \"level\",
      nodes.todo as todo,
      nodes.pos as pos,
      nodes.priority as priority,
      nodes.scheduled as scheduled,
      nodes.deadline as deadline,
      nodes.title as title,
      nodes.properties as properties,
      nodes.olp as olp,
      nodes.backlinkcount as backlinkcount,
      files.atime as atime,
      files.mtime as mtime,
      files.title as filetitle,
      tags.tag as tags,
      aliases.alias as aliases,
      '(' || group_concat(RTRIM (refs.\"type\", '\"') || ':' || LTRIM(refs.ref, '\"'), ' ') || ')' as refs
    FROM nodes
    LEFT JOIN files ON files.file = nodes.file
    LEFT JOIN tags ON tags.node_id = nodes.id
    LEFT JOIN aliases ON aliases.node_id = nodes.id
    LEFT JOIN refs ON refs.node_id = nodes.id
    GROUP BY nodes.id, tags.tag, aliases.alias )
  GROUP BY id, tags )
GROUP BY id")))
    (cl-loop for row in rows
             append (pcase-let* ((`(,id ,file ,file-title ,level ,todo ,pos ,priority ,scheduled
                                        ,deadline ,title ,properties ,olp ,atime ,mtime ,tags
                                        ,aliases ,refs (el-patch-add ,backlink-count))
                                  row)
                                 (all-titles (cons title aliases)))
                      (mapcar (lambda (temp-title)
                                (org-roam-node-create :id id
                                                      :file file
                                                      :file-title file-title
                                                      :file-atime atime
                                                      :file-mtime mtime
                                                      :level level
                                                      :point pos
                                                      :todo todo
                                                      :priority priority
                                                      :scheduled scheduled
                                                      :deadline deadline
                                                      :title temp-title
                                                      :aliases aliases
                                                      :properties properties
                                                      :olp olp
                                                      :tags tags
                                                      :refs refs
                                                      (el-patch-add :backlink-count backlink-count)))
                              all-titles)))))
;; Here stops

(el-patch-defun org-roam-node-visit (node &optional other-window force)
  "From the current buffer, visit NODE. Return the visited buffer.
Display the buffer in the selected window.  With a prefix
argument OTHER-WINDOW display the buffer in another window
instead.

If NODE is already visited, this won't automatically move the
point to the beginning of the NODE, unless FORCE iws non-nil. In
interactive calls FORCE always set to t."
  (interactive (list (org-roam-node-at-point t) current-prefix-arg t))
  (let ((buf (org-roam-node-find-noselect node force))
        (display-buffer-fn (if (el-patch-swap other-window mymy-org-roam-visit-node-other-window)
                               #'switch-to-buffer-other-window
                             #'pop-to-buffer-same-window)))
    (el-patch-swap (funcall display-buffer-fn buf)
                   (if (equal (org-roam-node-file node) (ignore-errors (org-roam-node-file (org-roam-node-at-point))))
                       (progn
                         (pop-to-buffer-same-window buf)
                         (org-show-entry))
                     (funcall display-buffer-fn buf)))
    (when (org-invisible-p) (org-show-context))
    buf))

(defcustom mymy-org-roam-node-insert-hook nil
  "Hook to run when an Org-roam node is inserted"
  :group 'org-roam
  :type 'hook)

(el-patch-cl-defun org-roam-node-insert (&optional filter-fn &key templates info)
  "Find an Org-roam node and insert (where the point is) an \"id:\" link to it.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
The TEMPLATES, if provided, override the list of capture templates (see
`org-roam-capture-'.)
The INFO, if provided, is passed to the underlying `org-roam-capture-'."
  (interactive)
  (unwind-protect
      ;; Group functions together to avoid inconsistent state on quit
      (atomic-change-group
        (let* (region-text
               beg end
               (_ (when (region-active-p)
                    (setq beg (set-marker (make-marker) (region-beginning)))
                    (setq end (set-marker (make-marker) (region-end)))
                    (setq region-text (org-link-display-format (buffer-substring-no-properties beg end)))))
               (node (org-roam-node-read region-text filter-fn))
               (description (or region-text
                                (org-roam-node-formatted node))))
          (if (org-roam-node-id node)
              (progn
                (when region-text
                  (delete-region beg end)
                  (set-marker beg nil)
                  (set-marker end nil))
                (insert (org-link-make-string
                         (concat "id:" (org-roam-node-id node))
                         description))
                (el-patch-add
                  (run-hook-with-args 'mymy-org-roam-node-insert-hook node))
                )
            (org-roam-capture-
             :node node
             :info info
             :templates templates
             :props (append
                     (when (and beg end)
                       (list :region (cons beg end)))
                     (list :insert-at (point-marker)
                           :link-description description
                           :finalize 'insert-link))))))
    (deactivate-mark)))
(defvar mymy-org-roam-entry-separation-tag ",")

(el-patch-defun org-roam-node--format-entry (template node &optional width)
  "Formats NODE for display in the results list.
WIDTH is the width of the results list.
TEMPLATE is the processed template used to format the entry."
  (pcase-let ((`(,tmpl . ,tmpl-width) template))
    (org-roam-format-template
     tmpl
     (lambda (field _default-val)
       (pcase-let* ((`(,field-name ,field-width) (split-string field ":"))
                    (getter (intern (concat "org-roam-node-" field-name)))
                    (field-value (funcall getter node)))
         (when (and (equal field-name "file")
                    field-value)
           (setq field-value (file-relative-name field-value org-roam-directory)))
         (when (and (equal field-name "olp")
                    field-value)
           (setq field-value (string-join field-value " > ")))
         (when (and field-value (not (listp field-value)))
           (setq field-value (list field-value)))
         (setq field-value (mapconcat
                            (lambda (v)
                              (concat (or (cdr (assoc field-name org-roam-node-template-prefixes))
                                          "")
                                      v))
                            field-value (el-patch-swap " " mymy-org-roam-entry-separation-tag)))
         (setq field-width (cond
                            ((not field-width)
                             field-width)
                            ((string-equal field-width "*")
                             (if width
                                 (- width tmpl-width)
                               tmpl-width))
                            ((>= (string-to-number field-width) 0)
                             (string-to-number field-width))))
         (when field-width
           (let* ((truncated (truncate-string-to-width field-value field-width 0 ?\s))
                  (tlen (length truncated))
                  (len (length field-value)))
             (if (< tlen len)
                 ;; Make the truncated part of the string invisible. If strings
                 ;; are pre-propertized with display or invisible properties, the
                 ;; formatting may get messed up. Ideally, truncated strings are
                 ;; not preformatted with these properties. Face properties are
                 ;; allowed without restriction.
                 (put-text-property tlen len 'invisible t field-value)
               ;; If the string wasn't truncated, but padded, use this string instead.
               (setq field-value truncated))))
         field-value)))))
(provide 'org-roam-patch)
