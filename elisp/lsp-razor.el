;; -*- lexical-binding: t; -*-
;; Razor commit ref: 08af6175499b0615a266b11c46e6220880a186d1 on https://github.com/dotnet/razor/tree/main

;; RazorLanguageFeatureBase at getProjection

;; DONE: Completion
;; DONE: Locations (goto reference, implementation, definition, etc)
;; TODO: Rename
;; TODO: Formatting
;; TODO: Code actions? (I mean, it looks like they work)
;; TODO: Hover? (This also seems to work)
;; TODO: Add provisional dot for expressions in html that in the projected document don't have dot, so that we can get provisional dot completion.
;; DONE: Virtual buffers
;; TODO: There is a problem with applying snippets, most probably because I deliberately didn't map them.
;; TODO: Make a middleware that maps locations and edits from csharp buffers to virtual csharp buffers.
;; TODO: Make a lsp-razor-completion-at-point function
;; TODO: RazorTranslateDiagnosticsEndpoint


(comment
 "Experiments"

 ;; Isolating requests for servers, so that a request isn't sent to every server
 (defun lsp-razor--ensure-activated-buffer (virtual-buffer mode)
   "Ensures that VIRTUAL-BUFFER's mode is MODE and that the LSP client is
active in that buffer"
   (with-current-buffer virtual-buffer
     (unless (equal major-mode mode)
       (funcall mode))
     (lsp)))

 (let ((buffer-file-name "virtual-csharp__virtual.cs"))
   (lsp))

 ;; TODO: Check out lsp-virtual buffers and how they can be implemented
 ;; here to see if they offer any improvement.

 ;; Using a hidden buffer instead of a string
 (let ((csharp-buffer (get-buffer-create (concat " *" "virtual-csharp__virtual.cs")))
       (buffer-file-name "virtual-csharp__virtual.cs")
       (start 0)
       (length 0)
       (new-text "\nConsole.WriteLine(\"HelloWorld\"")
       (mode 'csharp-ts-mode)
       )

   (with-current-buffer csharp-buffer
     (lsp-razor--ensure-activated-buffer csharp-buffer mode)
     (unless (equal start (+ start length))
       (delete-region start (+ start length)))
     (goto-char start)
     (insert new-text)
     )
   )
 )


;; (add-to-list 'lsp-language-id-configuration '(".*\\.cshtml$" . "aspnetcorerazor"))


(defconst lsp-razor-virtual-html-suffix
  "__virtual.html"
  ;; ""
  )

(defconst lsp-razor-virtual-csharp-suffix
  "__virtual.cs"
  )

(defvar lsp-razor--razor-file-extensions (list "razor" "cshtml")
  "List of razor filename extensions.")

(defun lsp-razor--is-razor-file ()
  (or (-some->> (buffer-file-name)
        (string-match-p (concat ".*\\.\\(" (s-join "\\|" lsp-razor--razor-file-extensions) "\\)\\'")))
      (lsp-find-workspace 'rzls (buffer-file-name))
      ))

(add-to-list 'lsp-language-id-configuration
             `(,(concat ".*\\.\\(" (s-join "\\|" lsp-razor--razor-file-extensions) "\\)\\'")
               . "aspnetcorerazor"))

(defun lsp-razor--is-virtual-csharp-file (uri)
  "Check if this URI is referencing a virtual csharp document."
  (s-ends-with? lsp-razor-virtual-csharp-suffix uri)
  )

(defun lsp-razor--is-virtual-html-file (uri)
  "Check if this URI is referencing a virtual html document."
  (s-ends-with? lsp-razor-virtual-html-suffix uri)
  )

(defun lsp-razor--get-razor-document-uri (uri)
  "Assuming URI is a virtual document uri, return the host document path."
  (->> uri
       (s-replace lsp-razor-virtual-html-suffix "")
       (s-replace lsp-razor-virtual-csharp-suffix "")
       ))


(defvar lsp-razor-rzls-test-path
  ;; (expand-file-name "~/.vscode/exutensions/ms-dotnettools.csharp-2.18.16-linux-x64/.razoromnisharp/rzls")
  ;; (expand-file-name "~/tmp/razor/artifacts/bin/rzls/Release/net8.0/rzls")
  ;; (expand-file-name "~/tmp/razor/artifacts/bin/rzls/Debug/net7.0/linux-x64/rzls")
  ;; (expand-file-name "~/tmp/razor/artifacts/bin/rzls/Release/net8.0/publish/rzls")
  (expand-file-name "~/tmp/razor/artifacts/bin/rzls/Release/net8.0/rzls")
  ;; (expand-file-name "~/tmp/razor/artifacts/bin/rzls/Debug/net8.0/rzls")
  ;; (expand-file-name "~/.vscode/extensions/ms-dotnettools.csharp-2.23.15-linux-x64/.razoromnisharp/rzls")
  "Version tags/Visual-Studio-2022-Version-17.8.5"
  )

;; Let's turn of this for a momment, because the razor plugin is giving problems with the normal razor.
(defvar lsp-razor-rzls-test-dll
  nil
  ;; (expand-file-name "~/.vscode/extensions/ms-dotnettools.csharp-2.23.15-linux-x64/.razoromnisharp/OmniSharpPlugin/Microsoft.AspNetCore.Razor.OmniSharpPlugin.dll")
  ;; (expand-file-name "~/tmp/OmniSharpPlugin/Microsoft.AspNetCore.Razor.OmniSharpPlugin.dll")
  )

(defvar lsp-razor-rzls-args (list "--CSharpVirtualDocumentSuffix"
                                  lsp-razor-virtual-csharp-suffix
                                  "--HtmlVirtualDocumentSuffix"
                                  lsp-razor-virtual-html-suffix
                                  "--ShowAllCSharpCodeActions"
                                  "true"
                                  "--SingleServerSupport"
                                  "true"
                                  "--SingleServerCompletionSupport"
                                  "true"
                                  )
  "Arguments for the razor language server."
  )

(defconst lsp-razor-language-kind-assoc
  (list '(1 . CSharp)
        '(2 . Html)
        '(3 . Razor))
  "Association list for the RazorLanguageKind enum."
  )

(defconst lsp-razor-language-uri-assoc
  (list `(CSharp . ,lsp-razor-virtual-csharp-suffix)
        `(Html . ,lsp-razor-virtual-html-suffix)
        ;; `(Html . "")
        )
  "Association list between language and suffix."
  )

(defconst lsp-razor--razor-html-scheme "razor-html"
  "Uri scheme for the projected html scheme")

(defconst lsp-razor--razor-csharp-scheme "virtualCSharp-razor"
  "Uri scheme for the projected csharp scheme")

;; Protocol

(defconst lsp-csharp--change-types
  (list "Unspecified"
        "Change"
        "Create"
        "Delete"
        "DirectoryDelete"))


(lsp-interface (omnisharp:SimpleFileRequest (:FileName) nil)
               ;; Equivalent to Point and TextDocument:Text
               (omnisharp:Request (:FileName :Buffer) (:Line :Column :Changes :ApplyChangesTogether))
               (omnisharp:UpdateBufferRequest (:FileName :Buffer) (:FromDisk :Line :Column :Changes :ApplyChangesTogether))
               ;; (omnisharp:ChangeBufferRequest (:FileName :StartLine :StartColumn :EndLine :EndColumn :NewText))
               (omnisharp:FileOpenRequest (:FileName) (:Buffer :Line :Column :Changes :ApplyChangesTogether))
               (omnisharp:FileCloseRequest (:FileName) (:Buffer :Line :Column :Changes :ApplyChangesTogether))
               (omnisharp:FilesChangedRequest (:FileName :ChangeType) (:Buffer :Line :Column :Changes :ApplyChangesTogether))
               )

(lsp-interface
 (rzls:UpdateBufferRequest (:hostDocumentFilePath :changes :previousWasEmpty) (:hostDocumentVersion :projectKeyId))
 (rzls:RazorLanguageQueryParams (:uri :position))
 (rzls:RazorMapToDocumentRangesParams (:kind :razorDocumentUri :projectedRanges) (:mappingBehavior)))

;; Feature base

(defun lsp-razor--language-query-params (&optional position uri)
  (lsp-make-rzls-razor-language-query-params :position (or position (lsp--cur-position))
                                             :uri (or uri (lsp--buffer-uri))))

(defun lsp-razor--language-query (&optional position uri)
  (with-lsp-workspaces (list (lsp-razor--razor-workspace))
    (lsp-request "razor/languageQuery"
                 (lsp-razor--language-query-params position uri))))

(defun lsp-razor--language-query-while-no-input (&optional position uri)
  (with-lsp-workspaces (list (lsp-razor--razor-workspace))
    (lsp-request-while-no-input "razor/languageQuery"
                                (lsp-razor--language-query-params position uri))))

(defun lsp-razor--language-query-async (callback error-handler &optional cancel-token position uri)
  (with-lsp-workspaces (list (lsp-razor--razor-workspace))
    (lsp-request-async "razor/languageQuery"
                       (lsp-razor--language-query-params position uri)
                       callback
                       :error-handler error-handler
                       :cancel-token cancel-token
                       )))

(defun lsp-razor--omnisharp-open-buffer (file-name &optional close)
  (apply #'lsp-request
         (if close
             (list "o#/close" (lsp-make-omnisharp-file-close-request file-name))
           (list "o#/open" (lsp-make-omnisharp-file-open-request file-name))
           )))

(defconst lsp-razor--csharp-server-ids
  (list 'omnisharp-razor 'omnisharp)
  "List of server-ids that identify a csharp language server."
  )

(defun lsp-razor--buffer-workspaces ()
  "Get the real list of workspaces for this session."
  (-some->> (lsp-session)
    (lsp-session-folder->servers)
    (gethash (lsp-workspace-root))))

(defun lsp-razor--csharp-workspaces ()
  "Get the reference for the csharp language servers"
  (-filter (-compose (-partial #'-contains? lsp-razor--csharp-server-ids)
                     #'lsp--client-server-id
                     #'lsp--workspace-client)
           (lsp-razor--buffer-workspaces)))

(defun lsp-razor--html-workspace ()
  "Get the reference for the html language server"
  (car (-filter (-compose (-partial #'eq 'razor-html-ls)
                          #'lsp--client-server-id
                          #'lsp--workspace-client)
                (lsp-razor--buffer-workspaces))))

(defun lsp-razor--razor-workspace ()
  "Get the reference for the razor language server"
  (car (-filter (-compose (-partial #'eq 'aspnetcorerazor)
                          #'lsp--client-server-id
                          #'lsp--workspace-client)
                (lsp-razor--buffer-workspaces))))

(defun lsp-razor--razor-and-html-workspace ()
  "Get the reference for the razor and html language servers"
  (-filter (-compose (-partial #'-contains? '(aspnetcorerazor razor-html-ls))
                     #'lsp--client-server-id
                     #'lsp--workspace-client)
           (lsp-razor--buffer-workspaces)))

(defun lsp-razor--html-and-csharp-workspaces ()
  "Get the reference for the csharp and html language servers"
  (-filter (-compose (-partial #'-contains? (cons 'razor-html-ls lsp-razor--csharp-server-ids))
                     #'lsp--client-server-id
                     #'lsp--workspace-client)
           (lsp-razor--buffer-workspaces)))

(defun lsp-razor--omnisharp-update-buffer (file-name buffer)
  (when-let ((csharp-workspace (car (lsp-razor--csharp-workspaces))))
    (with-lsp-workspace csharp-workspace
      ;; (lsp-request "o#/updatebuffer"
      ;;              (lsp-make-omnisharp-update-buffer-request
      ;;               :file-name file-name
      ;;               :buffer buffer))
      (lsp-request-async "o#/updatebuffer"
                         (lsp-make-omnisharp-update-buffer-request
                          :file-name file-name
                          :buffer buffer)
                         #'ignore
                         ;; This is a complicated matter, the virtual buffer
                         ;; and the host buffer are like separate entities
                         ;; that are connected through semantics different
                         ;; from emacs.
                         :mode 'detached
                         :error-handler
                         (lambda (error)
                           (lsp--error
                            "Failed to update csharp buffer. Error: %S" error))))))

(defvar lsp-razor--pending-changes
  (make-hash-table
   :test 'equal))

(defun lsp-razor--html-update-buffer (file-name buffer
                                                open?
                                                ;; changes
                                                version
                                                last-pos-max)
  (cl-pushnew
   (list :buffer buffer
         :open? open?
         ;; :changes changes
         :version version
         )
   (gethash file-name lsp-razor--pending-changes))

  (if-let ((workspace (lsp-razor--html-workspace)))
      (with-lsp-workspace workspace
        ;; (lsp-notify "textDocument/didChange"
        ;;             (list :textDocument (list :uri file-name
        ;;                                       :version version)
        ;;                   :contentChanges (vector (list :range (lsp--range (list :character 0 :line 0)
        ;;                                                                    last-pos-max)
        ;;                                                 :text buffer))))
        ;; (if open?
        ;;     (progn
        ;;       (lsp-notify "textDocument/didClose"
        ;;                   (list :textDocument
        ;;                         (list :uri file-name)))
        ;;       (lsp-notify "textDocument/didOpen"
        ;;                   (list :textDocument
        ;;                         (list :uri file-name
        ;;                               :languageId "html"
        ;;                               :version version
        ;;                               :text buffer))))
        ;;   (lsp-notify "textDocument/didChange"
        ;;               (list :textDocument (list :uri file-name
        ;;                                         :version version)
        ;;                     :contentChanges (vector (list :range (lsp--range (list :character 0 :line 0)
        ;;                                                                      last-pos-max)
        ;;                                                   :text buffer))))
        ;;   )

        (cl-loop for x in (gethash file-name lsp-razor--pending-changes)
                 do (-let (((&plist :buffer :open? :changes :version) x))
                      ;; (lsp-notify "textDocument/didChange"
                      ;;             (list :range (lsp--range (list :character 0 :line 0)
                      ;;                                      (with-temp-buffer
                      ;;                                        (insert buffer)
                      ;;                                        (lsp-point-to-position (point-max))
                      ;;                                        )
                      ;;                                      )
                      ;;                   :text buffer)
                      ;;             )
                      (if open?
                          (progn
                            (lsp-notify "textDocument/didClose"
                                        (list :textDocument
                                              (list :uri file-name)))
                            (lsp-notify "textDocument/didOpen"
                                        (list :textDocument
                                              (list :uri file-name
                                                    :languageId "html"
                                                    :version version
                                                    :text buffer))))
                        (lsp-notify "textDocument/didChange"
                                    (list :textDocument (list :uri file-name
                                                              :version version)
                                          :contentChanges (vector (list :range (lsp--range (list :character 0 :line 0)
                                                                                           last-pos-max)
                                                                        :text buffer))))
                        ;; (lsp-notify "textDocument/didChange"
                        ;;             changes)
                        )
                      )
                 finally (puthash file-name nil lsp-razor--pending-changes))
        )))

(defvar lsp-razor--last-projection nil
  "This variable is used for cache. So that functions that advice can share
information. It gets updated each time `lsp-razor--position-params' gets
called."
  )


(defun lsp-razor--position-params (&optional no-cache)
  "Get the position of the projected documets.

When NO-CACHE is t, the variable `lsp-razor--last-projection' will not be modified"
  (-let (((projection &as &plist :uri :position :languageKind)
          (lsp-razor--get-projection
           (lsp--buffer-uri)
           (lsp--cur-position))))
    (prog1 (list :position position
                 :textDocument (list :uri uri))
      (or no-cache (setq lsp-razor--last-projection projection)))))

(defun lsp-razor--hack-text-document-params-async (virtual-document-suffix)
  "Return an around advice that makes the function
`lsp--text-document-identifier' return the document with the
VIRTUAL-DOCUMENT-SUFFIX"
  (lambda (orig-f &rest rest)
    (lambda (result)
      (cl-flet ((lsp--text-document-identifier ()
                  (list :textDocument
                        (list :uri (lsp-razor--get-virtual-buffer-file-name
                                    (lsp--buffer-uri)
                                    virtual-document-suffix
                                    )))))
        (message "%S" (lsp--text-document-identifier))
        (apply orig-f rest)))))

(defun lsp-razor--hack-text-document-position-params-to-async (&optional mode)
  (lambda
    (orig-f &rest rest)
    (if (lsp-razor--is-razor-file)
        (with-lsp-workspace (lsp-razor--razor-workspace)
          (lsp-request-async "razor/languageQuery"
                             (lsp-razor--language-query-params)
                             (lambda (result)
                               (cl-flet ((lsp--text-document-position-params (&optional identifier position)
                                           result))
                                 (apply orig-f rest)))
                             :mode mode))
      (apply orig-f rest))))


;; (advice-add #'lsp--document-links
;;             :around (lsp-razor--hack-text-document-params-async
;;                      lsp-razor-virtual-html-suffix))

(advice-add #'lsp--document-highlight
            :around (lsp-razor--hack-text-document-position-params-to-async
                     'tick))

(advice-add #'lsp-eldoc-function
            :around (lsp-razor--hack-text-document-position-params-to-async
                     'tick))

(advice-add #'lsp-signature
            :around (lsp-razor--hack-text-document-position-params-to-async))

(advice-remove #'lsp-signature (lsp-razor--hack-text-document-position-params-to-async))




(defun lsp-razor--lsp--text-document-position-params-advice (old-func &rest args)
  "Intercept a call to `lsp--text-document-position-params' to return the
projected positions and documents."
  (if (lsp-razor--is-razor-file)
      (lsp-razor--position-params)
    (apply old-func args)))

(advice-add #'lsp--text-document-position-params :around #'lsp-razor--lsp--text-document-position-params-advice)

(comment
 (defun lsp-completion--to-internal (items)
   "Convert ITEMS into internal form."
   (--> items
        (-map (-lambda ((item &as &CompletionItem
                              :label
                              :filter-text?
                              :_emacsStartPoint start-point
                              :score?))
                `(:label ,(or (unless (lsp-falsy? filter-text?) filter-text?) label)
                         :item ,item
                         :start-point ,start-point
                         :score ,score?))
              it)))

 (list
  :uri (lsp-razor-projected-document-uri projected-document)
  :position projected-position
  :languageKind language))

;; I think is worth putting a "completion transformer" or something like
;; that as the property of a client. That way a client can define custom
;; logic for processing completion items.

(defun lsp-razor--offset-completion-list (items projected-position)
  (let* ((position (plist-get projected-position :position))
         (offset (- (plist-get position :character)
                    (plist-get (lsp--cur-position) :character))))
    (list
     (->> (car items)
          (--map (let ((it (copy-sequence it))
                       (bounds-start (or (cl-first (bounds-of-thing-at-point 'symbol))
                                         (point))))
                   (setf (-> it
                             (plist-get :textEdit)
                             (plist-get :range))
                         (-let (((&Range :start :end) (-> it
                                                          (plist-get :textEdit)
                                                          (plist-get :range))))
                           (lsp-make-range
                            :start (lsp-make-position
                                    :character (- (plist-get start :character) offset)
                                    :line (lsp--cur-line))
                            :end (lsp-make-position
                                  :character (- (plist-get end :character) offset)
                                  :line (lsp--cur-line)))))
                   ;; Re-calculate this part.
                   (plist-put it :_emacsStartPoint
                              (or (lsp-completion--guess-prefix it)
                                  bounds-start))))))))

(defun lsp-razor--lsp-completion--to-internal-advice (items)
  "Intercept a call to `lsp-completion--to-internal' to correctly offset completion items"
  (if (lsp-razor--is-razor-file)
      (lsp-razor--offset-completion-list items lsp-razor--last-projection)
    items))

(advice-add #'lsp-completion--to-internal :filter-args #'lsp-razor--lsp-completion--to-internal-advice)

(defun lsp-razor--lsp-completion-add-provisional-dot ()
  (lsp-completion--get-context trigger-chars same-session?)
  )


(comment
 (--map (plist-put it :a (1+ (plist-get it :a))) (list (list :a 1 :b 2 :c 3)))
 (setq lsp-completion-no-cache nil)
 (setq lsp-completion-no-cache t)

 (caadr lsp-completion--cache)
 )

(defun lsp-razor--virtual-document-uri (host-uri language)
  (concat host-uri
          (-> language
              (cl-assoc lsp-razor-language-uri-assoc)
              (cdr))))
(comment
 (->> (lsp-razor--position-params)
      (lsp-request "textDocument/hover"))

 (lsp-razor--language-query
  (lsp--cur-position)
  (lsp--buffer-uri)))

(defun lsp-razor--get-projected-document-uri (language uri)
  (-> language
      (cl-assoc lsp-razor-language-kind-assoc)
      (cdr)
      (->> (lsp-razor--virtual-document-uri uri)))
  )

(defun lsp-razor--get-projection (uri position)
  (let* ((query (lsp-razor--language-query
                 position
                 uri))
         (projected-position (plist-get query :position))
         (language (plist-get query :kind))
         (projected-document-uri (lsp-razor--get-projected-document-uri
                                  language uri)))
    (list
     :uri projected-document-uri
     :position projected-position
     :languageKind language)))

;; Locations

(defun lsp-razor--map-to-document-ranges (kind projected-ranges host-document-uri)
  (with-lsp-workspace (lsp-razor--razor-workspace)
    (lsp-request "razor/mapToDocumentRanges"
                 (lsp-make-rzls-razor-map-to-document-ranges-params
                  :kind kind
                  :projected-ranges projected-ranges
                  :razor-document-uri host-document-uri)))
  )


(lsp-defun lsp-razor--remap-file-location (projection
                                           (location &as &Location
                                                     :uri
                                                     :range))
  "Remap a LOCATION object to a real document location"
  (cond
   ((and (lsp-razor--is-virtual-html-file uri)
         (-> projection
             (plist-get :kind)
             (cl-assoc lsp-razor-language-kind-assoc)
             (eql 'Html)))
    (lsp-make-location
     :uri (lsp-razor--get-razor-document-uri uri)
     :range range))

   ((lsp-razor--is-virtual-csharp-file uri)
    (let* ((document-uri (lsp-razor--get-razor-document-uri uri))
           (remapped-range (-> (lsp-razor--map-to-document-ranges
                                (car (cl-rassoc 'CSharp lsp-razor-language-kind-assoc))
                                (vector range)
                                document-uri)
                               (plist-get :ranges)
                               (aref 0))))
      (lsp-make-location
       :uri document-uri
       :range remapped-range)))

   (t location)))

(defun lsp-razor--lsp--locations-to-xref-items-advice (locations)
  (if (lsp-razor--is-razor-file)
      ;; Why is this two lists deep?
      (progn
        (when (listp (caar locations))
          (setq locations (car locations)))
        (->> locations
             ;; As a call to locations-to-xref is always preceded by a call
             ;; to textParams, we can be sure that the projection is up to
             ;; date.
             (-map (-partial #'lsp-razor--remap-file-location lsp-razor--last-projection))
             (--filter (not (= -1 (-> it
                                      (plist-get :range)
                                      (plist-get :start)
                                      (plist-get :line)))))
             (list)))
    locations))


(advice-add #'lsp--locations-to-xref-items :filter-args #'lsp-razor--lsp--locations-to-xref-items-advice)

;; Completion

(comment
 (let ((lsp-response-timeout 60))
   (when-let ((document (buffer-file-name)))
     (let* ((trigger-chars (-> (lsp--capability-for-method "textDocument/completion")
                               (lsp:completion-options-trigger-characters?)))
            (bounds-start (or (cl-first (bounds-of-thing-at-point 'symbol))
                              (point)))
            (same-session? (and lsp-completion--cache
                                ;; Special case for empty prefix and empty result
                                (or (cl-second lsp-completion--cache)
                                    (not (string-empty-p
                                          (plist-get (cddr lsp-completion--cache) :prefix))))
                                (equal (cl-first lsp-completion--cache) bounds-start)
                                (s-prefix?
                                 (plist-get (cddr lsp-completion--cache) :prefix)
                                 (buffer-substring-no-properties bounds-start (point)))))
            )
       (lsp-razor--omnisharp-completions
        (lsp-completion--get-context
         trigger-chars
         same-session?))))))


;; Projected documents notifications

(comment
 (-> "/home/cheerio-pixel/Projects/csharp/Dummy - IntelliSenseTest/Views/Home/Index.cshtml"
     (lsp-razor--get-document)
     (lsp-razor-document-csharp-document)
     (lsp-razor-projected-document-content)
     ))


;; (defvar lsp-razor--documents
;;   (make-hash-table :test 'equal)
;;   )

(defun lsp-razor--get-virtual-buffer (path)
  "Get a reference to the virtual buffer that is associated with this PATH"
  (->> path
       (concat " * ")
       (get-buffer-create)))

(comment
 (defun lsp-razor--ensure-activated-buffer ()
   "Ensures that VIRTUAL-BUFFER's mode is MODE and that the LSP client is
active in that buffer"
   (set-auto-mode)
   (lsp)))

;; TODO: Kill virtual buffer after killing host buffer.
;; Let's check this out. If I kill a buffer, it will send a didClose
;; notification, if I open it again, will it send me the whole buffer
;; again?

(comment
 (let ((string "123"))
   (concat (substring string
                      0 1)
           "4"
           (substring string 2 3)
           )
   ))

(defun lsp-razor--get-virtual-buffer-file-name (host-document-path prefix)
  (concat host-document-path prefix))

(defun lsp-razor--apply-edit (update-request
                              virtual-buffer-prefix
                              &optional after-update-fn)
  "Apply edit for virtual buffer.

AFTER-UPDATE-FN is a function that takes the contents of the buffer and the position of the last character before making any changes."
  (-let (((&plist :hostDocumentFilePath :changes :previousWasEmpty)
          update-request))
    (let ((virtual-buffer-file-name (lsp-razor--get-virtual-buffer-file-name
                                     hostDocumentFilePath
                                     virtual-buffer-prefix))
          ;; Set after applying edits to buffer
          contents
          last-point-max
          )
      (with-current-buffer (lsp-razor--get-virtual-buffer virtual-buffer-file-name)
        (when previousWasEmpty
          ;; Clear whole buffer
          (erase-buffer)
          )

        (setq last-point-max (lsp-point-to-position (point-max)))
        (cl-loop for change across (reverse changes)
                 do (-let (((&plist :span (&plist :start :length)
                                    :newText
                                    )
                            change))
                      ;; point-min is 1, so start is starts at 1
                      (cl-incf start)
                      ;; Delete
                      (delete-region start (min (point-max) (+ start length)))
                      ;; Position
                      (goto-char start)
                      ;; Insert
                      (insert newText)))
        (when (and (-> changes length (> 0))
                   after-update-fn
                   (functionp after-update-fn))
          (setq contents (lsp--buffer-content))
          )
        ;; Maybe not necessary?
        ;; (lsp--flush-delayed-changes)
        )
      ;; Call outised of the buffer
      (when contents
        (funcall after-update-fn contents last-point-max)))))

(defun lsp-razor--try-remove-temporal-dot ()
  "Remove the "
  )

(defun lsp-razor--try-add-temporal-dot (point virtual-buffer-file-name)
  ""
  )

(defun lsp-razor--run-only-on-csharp-and-html (orig-f &rest args)
  (if (lsp-razor--is-razor-file)
      (let ((lsp--buffer-workspaces (lsp-razor--html-and-csharp-workspaces)))
        (apply orig-f args))
    (apply orig-f args)))

(advice-add #'lsp-signature :around #'lsp-razor--run-only-on-csharp-and-html)

(defun lsp-razor--lsp-completion-at-point-advice (result)
  (cl-callf
      (lambda (orig-f) (-partial #'lsp-razor--run-only-on-csharp-and-html orig-f))
      ;; Candidates function
      (nth 2 result)
    )
  (cl-callf
      (lambda (orig-f) (-partial #'lsp-razor--run-only-on-csharp-and-html orig-f))
      ;; :exit-function
      (nth 18 result)
    )
  result
  )






(advice-add #'lsp-completion-at-point :filter-return #'lsp-razor--lsp-completion-at-point-advice)
;; (advice-remove #'lsp-completion-at-point #'lsp-razor--run-only-on-csharp-and-html)


;; (defun lsp-razor--lsp--document-highlight-callback-advice (args)
;;   (message "%S" args)
;;   args
;;   )

;; (advice-add #'lsp--document-highlight-callback :filter-args #'lsp-razor--lsp--document-highlight-callback-advice)
;; (advice-remove #'lsp--document-highlight-callback #'lsp-razor--lsp--document-highlight-callback-advice)


(defun lsp-razor-update-csharp-buffer (_workspace update-request)
  ;; (cl-pushnew update-request mymy-lsp-razor-edits)
  (lsp-razor--apply-edit update-request lsp-razor-virtual-csharp-suffix
                         (lambda (buffer-contents _)
                           (lsp-razor--omnisharp-update-buffer
                            (concat (plist-get update-request :hostDocumentFilePath)
                                    lsp-razor-virtual-csharp-suffix)
                            buffer-contents)))
  ;; TODO: Handle this nicely
  (comment
   (-let ()
     (lsp-razor--try-add-temporal-dot
      ;; 
      (lsp-razor--position-params)
      )
     ))
  ;; (cl-pushnew update-request mymy-lsp-razor-edits)
  ;; (lsp-razor-update--buffer
  ;;  _workspace update-request
  ;;  #'lsp-razor-document-csharp-document
  ;;  (lambda (projected-document)
  ;;    (setf (lsp-razor-document-csharp-document (gethash (plist-get update-request :hostDocumentFilePath)
  ;;                                                       lsp-razor--documents))
  ;;          projected-document))
  ;;  "CSharp"
  ;;  (lambda ()
  ;;    (let ((csharp-document (-> update-request
  ;;                               (plist-get :hostDocumentFilePath)
  ;;                               lsp-razor--get-document
  ;;                               lsp-razor-document-csharp-document)))
  ;;      (lsp-razor--omnisharp-update-buffer
  ;;       (lsp-razor-projected-document-path csharp-document)
  ;;       (lsp-razor-projected-document-content csharp-document)))))
  )

(setq lsp-razor--request-list '())

(defun lsp-razor-update-html-buffer (_workspace update-request)
  (cl-pushnew update-request lsp-razor--request-list)
  (lsp-razor--apply-edit update-request lsp-razor-virtual-html-suffix
                         (lambda (buffer-contents last-point-max)
                           (lsp-razor--html-update-buffer
                            (lsp--path-to-uri (concat (plist-get update-request :hostDocumentFilePath)
                                                      lsp-razor-virtual-html-suffix))
                            buffer-contents
                            (plist-get update-request :previousWasEmpty)
                            ;; (lsp-razor--update-request->content-changes
                            ;;  update-request
                            ;;  lsp-razor-virtual-html-suffix
                            ;;  )
                            (plist-get update-request :hostDocumentVersion)
                            last-point-max)))
  ;; (lsp-razor-update--buffer
  ;;  _workspace update-request
  ;;  #'lsp-razor-document-html-document
  ;;  (lambda (projected-document)
  ;;    (lsp-razor-document-html-document
  ;;     (gethash (plist-get update-request :hostDocumentFilePath)
  ;;              lsp-razor--documents))
  ;;    projected-document)
  ;;  "HTML")
  )

;; (lsp-razor--update-request->content-changes
;;  '( :hostDocumentVersion 8
;;     :projectKeyId "/tmp/5fecaf8dd6b7498db8eed235408061bc/__MISC_RAZOR_PROJECT__/"
;;     :hostDocumentFilePath "/home/cheerio-pixel/Projects/csharp/ITLA_Programacion3/Asignacion2ItlaTVPlus/ItlaTVPlus.WebApp/Views/Home/Index.cshtml"
;;     :changes [(:span (:start 108 :length 2)
;;                      :newText "
;; ")]
;;     :previousWasEmpty nil)
;;  lsp-razor-virtual-html-suffix
;;  )

;; '(:textDocument (:uri "/home/cheerio-pixel/Projects/csharp/ITLA_Programacion3/Asignacion2ItlaTVPlus/ItlaTVPlus.WebApp/Views/Home/Index.cshtml__virtual.html"
;;                       :version 8)
;; 		:contentChanges [(:range (:start (:line 9 :character 0)
;;                                                  :end (:line 10 :character 1))
;;                                          :text "
;; ")])


(defun lsp-razor--update-request->content-changes (update-request virtual-prefix)
  "Convert an UpdateRequest to didChange parameters.  Assumming update-request was applied on buffer."
  (let* ((document-file-path (lsp-razor--get-virtual-buffer-file-name
                              (plist-get update-request :hostDocumentFilePath)
                              virtual-prefix))
         (changes (plist-get update-request :changes))
         (previous-was-empty (plist-get update-request :previousWasEmpty))
         (versioned-text-document-identifier
          `( :uri ,document-file-path
             :version ,(plist-get update-request :hostDocumentVersion)))
         (content-changes
          (with-current-buffer (lsp-razor--get-virtual-buffer document-file-path)
            (vconcat (mapcar (lambda (change)
                               (let* ((span (plist-get change :span))
                                      (start (1+ (plist-get span :start)))
                                      (length (plist-get span :length))
                                      (new-text (plist-get change :newText)))
                                 (if (and start length)
                                     `( :range ( :start ,(lsp-point-to-position
                                                          start
                                                          )
                                                 :end ,(lsp-point-to-position
                                                        (+ start length)
                                                        ))
                                        :rangeLength ,(length new-text)
                                        :text ,new-text)
                                   `(:text ,new-text))))
                             changes)))))
    (list :textDocument versioned-text-document-identifier
          :contentChanges content-changes)))

(comment
 (->> lsp-razor--request-list
      (reverse)
      (--map (lsp-razor--update-request->content-changes it lsp-razor-virtual-html-suffix))
      )
 )


;; Too slow TODO
;; I think it will be wise to advice lsp--text-document-code-action-params
;; to work with this guy
(defun lsp-razor--provide-code-actions-request-handler (_workspace request)
  (-let (((&plist :codeActionParams :languageKind) request))
    (cond ((-> languageKind
               (cl-assoc lsp-razor-language-kind-assoc)
               (cdr) (eq 'CSharp))
           ;; Convert to vector since this is going to be a response.
           (with-lsp-workspace (car (lsp-razor--csharp-workspaces))
             (->> (lsp-request
                            "textDocument/codeAction"
                            (plist-put codeActionParams
                                       :textDocument
                                       (lsp-make-text-document-identifier
                                        :uri
                                        (lsp-razor--virtual-document-uri
                                         (-> codeActionParams
                                             (plist-get :textDocument)
                                             (plist-get :uri))
                                         'CSharp)))
                            :no-merge t)
                  (cdar))))
          (t []))))

(defun lsp-razor--run-only-on-razor-and-html (orig-f &rest args)
  (if (lsp-razor--is-razor-file)
      (let* (;; (lsp--cur-workspace (lsp-razor--razor-workspace))
             ;; On lsp-on-change, the lsp--cur-workspace is set to nil
             ;; locally. So we have to overwrite the lsp
             ;; lsp--buffer-workspaces to be sure
             ;; (lsp--cur-workspace nil)
             (lsp--buffer-workspaces (lsp-razor--razor-and-html-workspace))
             )
        (apply orig-f args))
    (apply orig-f args)))

(defconst lsp-razor--run-only-on-razor-and-html (list
                                                 #'lsp-on-change
                                                 #'lsp--before-save
                                                 #'lsp--text-document-did-close
                                                 #'lsp--text-document-did-open
                                                 #'lsp--on-auto-save
                                                 #'lsp-on-save
                                                 #'lsp--flush-delayed-changes
                                                 ))

(dolist (f lsp-razor--run-only-on-razor-and-html)
  (advice-add f :around #'lsp-razor--run-only-on-razor-and-html))

;; (dolist (f lsp-razor--run-only-on-razor-and-html)
;;   (advice-remove f #'lsp-razor--run-only-on-razor-and-html))

;; (defun lsp-razor--run-only-on-razor (orig-f &rest args)
;;   (if (lsp-razor--is-razor-file)
;;       (let* ((lsp--cur-workspace (lsp-razor--razor-workspace))
;;              ;; For some reason, lsp--cur-workspace is not being used by
;;              ;; func lsp-workspaces
;;              (lsp--buffer-workspaces (list lsp--cur-workspace))
;;              )
;;         (apply orig-f args))
;;     (apply orig-f args)))

;; (defconst lsp-razor--run-only-on-razor (list
;;                                         #'lsp-on-change
;;                                         #'lsp--before-save
;;                                         #'lsp--text-document-did-close
;;                                         #'lsp--on-auto-save
;;                                         #'lsp-on-save
;;                                         #'lsp--flush-delayed-changes
;;                                         ))

;; (dolist (f lsp-razor--run-only-on-razor)
;;   (advice-add f :around #'lsp-razor--run-only-on-razor))

(defun lsp-razor-handle-razor-completion (request _workspace)
  (with-lsp-workspace _workspace
    (lsp-request "textDocument/completion" request)
    )
  )


(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda ()
                                          (append (list lsp-razor-rzls-test-path)
                                                  lsp-razor-rzls-args
                                                  )
                                          ))
  :activation-fn (lsp-activate-on "aspnetcorerazor")
  :server-id 'aspnetcorerazor
  :priority -2
  :add-on? t
  ;; TODO: Handle this
  ;; :async-request-handlers (ht ("razor/provideCodeActions" #'lsp-razor--provide-code-actions-request-handler))
  :request-handlers (ht ("razor/htmlOnTypeFormatting" #'ignore)
                        ("textDocument/onTypeFormatting" #'ignore)
                        ("razor/provideCodeActions" #'ignore)
                        ("razor/completion" #'lsp-razor-handle-razor-completion)
                        ;; ("razor/provideCodeActions" #'lsp-razor--provide-code-actions-request-handler)
                        )
  :notification-handlers (ht ("razor/updateCSharpBuffer" #'lsp-razor-update-csharp-buffer)
                             ("razor/updateHtmlBuffer" #'lsp-razor-update-html-buffer))
  ))

(lsp-register-client
 (make-lsp-client :new-connection
                  (lsp-stdio-connection
                   #'(lambda ()
                       (append
                        (list (lsp-csharp--language-server-path) "-lsp"
                              ;; "-l" "Debug"
                              )
                        (when lsp-razor-rzls-test-dll
                          (list "--plugin" lsp-razor-rzls-test-dll)
                          )
                        (when lsp-csharp-solution-file
                          (list "-s" (expand-file-name lsp-csharp-solution-file)))))
                   #'(lambda ()
                       (when-let ((binary (lsp-csharp--language-server-path)))
                         (f-exists? binary))))
                  :activation-fn (lsp-activate-on "csharp" "aspnetcorerazor")
                  :server-id 'omnisharp-razor
                  :priority 0
                  :uri->path-fn #'lsp-csharp--uri->path-fn
                  :action-handlers (ht ("omnisharp/client/findReferences" 'lsp-csharp--action-client-find-references))
                  :notification-handlers (ht ("o#/projectadded" 'ignore)
                                             ("o#/projectchanged" 'ignore)
                                             ("o#/projectremoved" 'ignore)
                                             ("o#/packagerestorestarted" 'ignore)
                                             ("o#/msbuildprojectdiagnostics" 'ignore)
                                             ("o#/packagerestorefinished" 'ignore)
                                             ("o#/unresolveddependencies" 'ignore)
                                             ("o#/error" 'lsp-csharp--handle-os-error)
                                             ("o#/testmessage" 'lsp-csharp--handle-os-testmessage)
                                             ("o#/testcompleted" 'lsp-csharp--handle-os-testcompleted)
                                             ("o#/projectconfiguration" 'ignore)
                                             ("o#/projectdiagnosticstatus" 'ignore)
                                             ("o#/backgrounddiagnosticstatus" 'ignore)
                                             )
                  :download-server-fn #'lsp-csharp--omnisharp-download-server))


;; (mymy-kill-new (lsp--server-capabilities))

(comment
 
 )

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda ()
                                     (cons (lsp-package-path 'html-language-server)
                                           lsp-html-server-command-args)))
                  :activation-fn (lsp-activate-on "aspnetcorerazor")
                  :priority -4
                  :completion-in-comments? t
                  ;; Normal html, but an addon for html
                  :server-id 'razor-html-ls
                  :add-on? t
                  :initialized-fn (lambda (w)
                                    (with-lsp-workspace w
                                      (lsp--set-configuration
                                       (lsp-configuration-section "html"))))
                  :download-server-fn (lambda (_client callback error-callback _update?)
                                        (lsp-package-ensure
                                         'html-language-server callback
                                         error-callback))))


(comment
 (with-lsp-workspace (lsp-razor--razor-workspace)
   (let* ((bounds-start (or (cl-first (bounds-of-thing-at-point 'symbol))
                            (point)))
          (same-session? (and lsp-completion--cache
                              ;; Special case for empty prefix and empty result
                              (or (cl-second lsp-completion--cache)
                                  (not (string-empty-p
                                        (plist-get (cddr lsp-completion--cache) :prefix))))
                              (equal (cl-first lsp-completion--cache) bounds-start)
                              (s-prefix?
                               (plist-get (cddr lsp-completion--cache) :prefix)
                               (buffer-substring-no-properties bounds-start (point)))))
          (trigger-chars (-> (lsp--capability-for-method "textDocument/completion")
                             (lsp:completion-options-trigger-characters?))))
     (lsp-request-while-no-input
      "textDocument/completion"
      (plist-put (list :textDocument (lsp--text-document-identifier)
                       :position (lsp--cur-position))
                 :context (lsp-completion--get-context trigger-chars same-session?)))))

 (lsp-completion-at-point)
 (lsp--workspace-client)

 (lsp--cur-workspace)
 (mymy-kill-new (lsp-workspaces))

 (mymy-kill-new
  (run-hook-wrapped 'completion-at-point-functions
                    #'completion--capf-wrapper 'all))

 (mymy-kill-new
  (lsp-request "textDocument/completion"
               (plist-put (lsp--text-document-position-params)
                          :context (lsp-completion--get-context
                                    (->
                                     (lsp--capability-for-method
                                      "textDocument/completion")
                                     (lsp:completion-options-trigger-characters?))
                                    (and lsp-completion--cache
                                         ;; Special case for empty prefix and empty result
                                         (or (cl-second
                                              lsp-completion--cache)
                                             (not (string-empty-p
                                                   (plist-get
                                                    (cddr lsp-completion--cache)
                                                    :prefix))))
                                         (equal (cl-first
                                                 lsp-completion--cache)
                                                bounds-start)
                                         (s-prefix?
                                          (plist-get
                                           (cddr lsp-completion--cache)
                                           :prefix)
                                          (buffer-substring-no-properties
                                           bounds-start
                                           (point))))))))

 (mymy-kill-new (lsp--text-document-position-params))

 (with-lsp-workspace (lsp-razor--razor-workspace)

   (let ((bounds-start (or (cl-first (bounds-of-thing-at-point 'symbol))
                           (point))))
     (lsp-request-while-no-input
      "textDocument/completion"
      (plist-put
       (list :textDocument (lsp--text-document-identifier)
             :position (lsp--cur-position))
       :context (lsp-completion--get-context
                 (->
                  (lsp--capability-for-method
                   "textDocument/completion")
                  (lsp:completion-options-trigger-characters?))
                 (and lsp-completion--cache
                      ;; Special case for empty prefix and empty result
                      (or (cl-second
                           lsp-completion--cache)
                          (not (string-empty-p
                                (plist-get
                                 (cddr lsp-completion--cache)
                                 :prefix))))
                      (equal (cl-first
                              lsp-completion--cache)
                             bounds-start)
                      (s-prefix?
                       (plist-get
                        (cddr lsp-completion--cache)
                        :prefix)
                       (buffer-substring-no-properties
                        bounds-start
                        (point))))))))
   )

 (with-lsp-workspace (lsp-razor--razor-workspace)
   (->
    (lsp--capability-for-method
     "textDocument/completion")
    ;; (lsp:completion-options-trigger-characters?)
    ))

 (with-lsp-workspace (lsp-razor--razor-workspace)
   (->
    (lsp--capability-for-method
     "razor/completion")
    ;; (lsp:completion-options-trigger-characters?)
    ))

 (with-lsp-workspaces (lsp-razor--csharp-workspaces)
   (->
    (lsp--capability-for-method
     "textDocument/completion")
    ;; (lsp:completion-options-trigger-characters?)
    ))

 (with-lsp-workspace (lsp-razor--razor-workspace)
   (->
    (lsp--capability-for-method
     "textDocument/completion")
    (lsp:completion-options-trigger-characters?)))





 (let ((completions '( :isIncomplete nil
                       :items (:items [(:label "cache"
                                               :kind 25
                                               :commitCharacters [" " ">"]
                                               :data (:_resultId 11))
                                       (:label "environment"
                                               :kind 25
                                               :commitCharacters [" " ">"]
                                               :data (:_resultId 11))
                                       (:label "persist-component-state"
                                               :kind 25
                                               :commitCharacters [" " ">"]
                                               :data (:_resultId 11))]))))
   (lsp-make-completion-list
    :isIncomplete (plist-get completions :isIncomplete)
    :items (plist-get (plist-get completions :items) :items)
    ))

 (mymy-kill-new (lsp-completion-at-point)))

(provide 'lsp-razor)

