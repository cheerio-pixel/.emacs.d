;; -*- lexical-binding: t; -*-
(require 'org-roam)
(require 'el-patch)
(require 'cl-lib)

;; Here begins all that i have done to have backlink-counts
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
           (el-patch-add (backlinkcount (caar (org-roam-db-query
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
               scheduled deadline title properties olp (el-patch-add backlinkcount))))))


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
               (el-patch-add (backlinkcount (caar (org-roam-db-query
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
                   scheduled deadline title properties olp (el-patch-add backlinkcount)))
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

(cl-defmethod org-roam-populate ((node org-roam-node))
  "Populate NODE from database.
Uses the ID, and fetches remaining details from the database.
This can be quite costly: avoid, unless dealing with very few
nodes."
  (when-let ((node-info (car (org-roam-db-query [:select [file level pos todo priority
                                                          scheduled deadline title properties olp backlinkcount]
                                                 :from nodes
                                                 :where (= id $s1)
                                                 :limit 1]
                                                (org-roam-node-id node)))))
    (pcase-let* ((`(,file ,level ,pos ,todo ,priority ,scheduled ,deadline ,title ,properties ,olp ,backlink-count) node-info)
                 (`(,atime ,mtime ,file-title) (car (org-roam-db-query [:select [atime mtime title]
                                                                        :from files
                                                                        :where (= file $s1)]
                                                                       file)))
                 (tag-info (mapcar #'car (org-roam-db-query [:select [tag] :from tags
                                                             :where (= node-id $s1)]
                                                            (org-roam-node-id node))))
                 (alias-info (mapcar #'car (org-roam-db-query [:select [alias] :from aliases
                                                               :where (= node-id $s1)]
                                                              (org-roam-node-id node))))
                 (refs-info (mapcar #'car (org-roam-db-query [:select [ref] :from refs
                                                              :where (= node-id $s1)]
                                                             (org-roam-node-id node)))))
      (setf (org-roam-node-file node) file
            (org-roam-node-file-title node) file-title
            (org-roam-node-file-atime node) atime
            (org-roam-node-file-mtime node) mtime
            (org-roam-node-level node) level
            (org-roam-node-point node) pos
            (org-roam-node-todo node) todo
            (org-roam-node-priority node) priority
            (org-roam-node-scheduled node) scheduled
            (org-roam-node-deadline node) deadline
            (org-roam-node-title node) title
            (org-roam-node-properties node) properties
            (org-roam-node-olp node) olp
            (org-roam-node-tags node) tag-info
            (org-roam-node-refs node) refs-info
            (org-roam-node-aliases node) alias-info
            ;; (org-roam-node-backlink-count node) backlink-count
            )))
  node)

;; Here stops

(el-patch-defun org-roam-db-update-file (&optional file-path no-require)
  "Update Org-roam cache for FILE-PATH.

If the file does not exist anymore, remove it from the cache.

If the file exists, update the cache with information.

If NO-REQUIRE, don't require optional libraries. Set NO-REQUIRE
when the libraries are already required at some toplevel, e.g.
in `org-roam-db-sync'."
  (setq file-path (or file-path (buffer-file-name (buffer-base-buffer))))
  (let ((content-hash (org-roam-db--file-hash file-path))
        (db-hash (caar (org-roam-db-query [:select hash :from files
                                           :where (= file $s1)] file-path)))
        info)
    (unless (string= content-hash db-hash)
      (unless no-require
        (org-roam-require '(org-ref oc)))
      (org-roam-with-file file-path nil
        (emacsql-with-transaction (org-roam-db)
          (save-excursion
            (org-set-regexps-and-options 'tags-only)
            (org-refresh-category-properties)
            (org-roam-db-clear-file)
            (org-roam-db-insert-file)
            (org-roam-db-insert-file-node)
            (setq org-outline-path-cache nil)
            (org-roam-db-map-nodes
             (list ;; (el-patch-add #'mymy-org-roam-db-clear-node-data)
              #'org-roam-db-insert-node-data
              #'org-roam-db-insert-aliases
              #'org-roam-db-insert-tags
              #'org-roam-db-insert-refs
              ))
            (setq org-outline-path-cache nil)
            (setq info (org-element-parse-buffer))
            (org-roam-db-map-links
             (list #'org-roam-db-insert-link))
            (when (fboundp 'org-cite-insert)
              (require 'oc)             ;ensure feature is loaded
              (org-roam-db-map-citations
               info
               (list #'org-roam-db-insert-citation)))))))))

;; (org-map-entries #'mymy-org-roam-update-headline nil 'tree)
(defun mymy-org-roam-update-headline ()
  (mymy-org-roam-db-clear-node-data)
  (org-roam-db-insert-node-data)
  (org-roam-db-insert-tags))

(defun mymy-org-roam-db-clear-node-data (&optional id)
  "Clear headline from cache"
  (let ((id (or id (org-id-get))))
    (org-roam-db-query [:delete :from nodes
                        :where (= id $s1)]
                       id)))

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
