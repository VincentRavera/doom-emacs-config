;;; go/config.el -*- lexical-binding: t; -*-

(after! (:and go-mode lsp)
  (setq lsp-gopls-staticcheck t)
  (setq lsp-eldoc-render-all t)
  (setq lsp-gopls-complete-unimported t)
  )
