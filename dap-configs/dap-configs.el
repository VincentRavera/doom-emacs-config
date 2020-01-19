;; load UI
;; https://github.com/emacs-lsp/dap-mode#dap-mode-configuration
(dap-mode 1)
(dap-ui-mode 1)
;; enables mouse hover support
(dap-tooltip-mode 1)
;; use tooltips for mouse hover
;; if it is not enabled `dap-mode' will use the minibuffer.
(tooltip-mode 1)
(require 'dap-python)
