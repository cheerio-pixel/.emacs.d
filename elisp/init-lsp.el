
;;; Commentary
;; All emacs lisp code for making lsp work

(require 'use-package)

;;** lsp
(use-package dap-mode
  :disabled
  :straight (:host github :repo "emacs-lsp/dap-mode" :files ("*.el"))
  :config

  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)

  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))

  (require 'dap-ui)

  (setq dap-internal-terminal #'dap-internal-terminal-vterm)

  (el-patch-defun dap-debug (debug-args)
    "Run debug configuration DEBUG-ARGS.

If DEBUG-ARGS is not specified the configuration is generated
after selecting configuration template.

:dap-compilation specifies a shell command to be run using
`compilation-start' before starting the debug session. It could
be used to compile the project, spin up docker, ...."
    (interactive (list (-> (dap--completing-read "Select configuration template: "
                                                 (-mapcat #'funcall dap-launch-configuration-providers)
                                                 'cl-first nil t)
                           cl-rest
                           copy-tree)))
    ;; NOTE: the launch configuration must be expanded *before* being passed to a
    ;; debug provider. This is because some debug providers (e.g. dap-python) pass
    ;; some fields of DEBUG-ARGS as shell arguments in :program-to-launch and try
    ;; very hard to quote them. Because of this, `dap-start-debugging' cannot
    ;; expand them properly. Any python configuration that uses variables in :args
    ;; will fail.
    (let* ((debug-args (dap-variables-expand-in-launch-configuration debug-args))
           (taskConfigurations (dap-tasks-configuration-get-all))
           (launch-args (or (-some-> (plist-get debug-args :type)
                              (gethash dap--debug-providers)
                              (funcall debug-args))
                            (user-error "Have you loaded the `%s' specific dap package?"
                                        (or (plist-get debug-args :type)
                                            (user-error "%s does not specify :type" debug-args)))))
           (cb (lambda ()
                 (if (functionp launch-args)
                     (funcall launch-args #'dap-start-debugging-noexpand)
                   (dap-start-debugging-noexpand launch-args)))))
      (-if-let ((&plist :dap-compilation) launch-args)
          ;;                                      MAY I ASK WHY THIS HAPPENS TO ME?
          (dap-debug-run-task (el-patch-wrap 1 0 (list `(:cwd ,(or (plist-get launch-args :dap-compilation-dir)
                                                                   (lsp-workspace-root)
                                                                   default-directory)
                                                              :command ,dap-compilation
                                                              :label ,(truncate-string-to-width dap-compilation 20)))) cb)
        (-if-let ((&plist :preLaunchTask) launch-args)
            (let* ((task (dap-tasks-get-configuration-by-label preLaunchTask))
                   (tasks (dap-tasks-configuration-get-depends task)))
              (if tasks
                  (dap-debug-run-task tasks cb)
                (user-error "No valid tasks found labelled \"%s\". Please check your tasks.json" preLaunchTask)))
          (funcall cb)))))
  )

(use-package lsp-mode
  :disabled
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         (js-mode . lsp)
         ;; (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-signature-mode))
  :config
  ;; For debugging
  ;; (setq lsp-log-io t)
  ;; (setq lsp-log-io nil)
  ;; (setq lsp-log-io-allowlist-methods (list))
  ;; Please forgive my soul for being foolish
  ;; (setq lsp-keep-workspace-alive t)
  ;; (lsp-register-custom-settings '(("omnisharp.enableImportCompletion" t)))
  ;; (setq lsp-completion-provider :none)
  (setq lsp-completion-provider :none)
  ;; (setq lsp-enable-folding nil)
  ;; (setq lsp-diagnostic-package :none)
  (setq lsp-diagnostic-package :auto)
  ;; Better performance
  (setq lsp-enable-symbol-highlighting nil)

  (setq lsp-vetur-format-default-formatter-css "none")
  (setq lsp-vetur-format-default-formatter-html "none")
  (setq lsp-vetur-format-default-formatter-js "none")
  (setq lsp-vetur-validation-template nil)

  ;; lsp-enable-snippet
  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/

  ;; And by default they have to put the most uncomfortable position.
  (setq lsp-lens-place-position 'above-line)
  (bind-key "M-RET" #'lsp-signature-activate 'lsp-mode-map)
  (with-eval-after-load 'which-key
    (lsp-enable-which-key-integration t))
  (with-eval-after-load 'csharp-mode
    (add-hook 'csharp-mode-hook #'lsp))

  (with-eval-after-load 'dap-mode
    ;; Enabling only some features
    ;; (setq dap-auto-configure-features '(sessions locals controls tooltip))
    (require 'dap-netcore)
    ;; (dap-netcore--debugger-install)
    (setq dap-netcore-download-url "https://github.com/Samsung/netcoredbg/releases/download/2.2.0-974/netcoredbg-linux-amd64.tar.gz")
    (dap-register-debug-template
     "NetCoreDbg::Launch (MyMy3)"
     (list :type "coreclr"
           :request "launch"
           :mode "launch"
           :name "NetCoreDbg::Launch"
           :console "externalTerminal"
           :dap-compilation "dotnet build"))))

(use-package lsp-ui
  :disabled
  :config (setq lsp-ui-sideline-show-hover t
                lsp-ui-sideline-delay 0.5
                lsp-ui-doc-delay 0.5
                lsp-ui-sideline-ignore-duplicates t
                lsp-ui-doc-position 'bottom
                lsp-ui-doc-alignment 'frame
                lsp-ui-doc-header nil
                lsp-ui-doc-include-signature t
                lsp-ui-doc-use-childframe t)
  ;; lsp-doc-show
  :commands lsp-ui-mode)

(use-package lsp-pyright
  :disabled
  :after lsp-mode
  :config
  (add-hook 'python-mode-hook (lambda () (require 'lsp-pyright)))
  (add-hook 'python-mode-hook #'lsp))

(use-package lsp-haskell
  :disabled
  ;; :defer 5
  :after lsp-mode
  ;; Uncontable tales i have of how this monster have ruined my day, not
  ;; because of itself, but because Of how much ram it needs and how my
  ;; little School-gorverment-given computer hogs from the effort of
  ;; keeping this thing afloat
  :config
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp)
  )

(use-package lsp-java
  :disabled
  :after lsp-mode
  :init
  (defun my/java-mode-hook ()
    (setq-local lsp-enable-indentation nil)
    (lsp)
    )
  (add-hook 'java-mode-hook #'my/java-mode-hook)
  (setenv "JAVA_HOME" "/usr/lib/jvm/java-17-openjdk/")
  (setq lsp-java-java-path
        (if (getenv "JAVA_HOME")
            (concat (getenv "JAVA_HOME") "bin/java")
          "java"))

  (setq lsp-java-configuration-runtimes
        `[(:name "JavaSE-17"
                 :path ,(getenv "JAVA_HOME")
                 :default t)
          (:name "JavaSE-11"
                 :path "/usr/lib/jvm/java-11-openjdk")]
        )
  (setq lsp-java-imports-gradle-wrapper-checksums
        [(:sha256: "a8451eeda314d0568b5340498b36edf147a8f0d692c5ff58082d477abe9146e4"
                   :allowed: t)])
  )

(with-eval-after-load 'kotlin-ts-mode
  (add-hook 'kotlin-ts-mode-hook 'lsp))

(with-eval-after-load 'vue-ts-mode
  (add-hook 'vue-ts-mode-hook #'lsp))

(with-eval-after-load 'vue-mode
  (add-hook 'vue-mode-hook #'lsp))

(with-eval-after-load 'consult
  (use-package consult-lsp))



(provide 'init-lsp)
