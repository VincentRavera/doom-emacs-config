;;; dap/config.el -*- lexical-binding: t; -*-

(after! dap-mode
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (dap-ui-mode 1)
  ;; enables mouse hover support
  (dap-tooltip-mode 1)
  ;; use tooltips for mouse hover
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  (tooltip-mode 1)
  ;; displays floating panel with debug buttons
  ;; requies emacs 26+
  (dap-ui-controls-mode 1)
  )

(after! (:and dap-mode java-mode)
  (require 'dap-java)
  )
(after! (:and dap-mode python-mode)
  (require 'dap-python)
  )
(after! (:and dap-mode go-mode)
  (require 'dap-go)
  ;; (setq dap-go-debug-path "~/.emacs.d/.local/etc/dap-extension/vscode/golang.go/extension/dist/goMain.js")
  )
