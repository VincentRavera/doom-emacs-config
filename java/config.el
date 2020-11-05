;;; java/config.el -*- lexical-binding: t; -*-

(setq lsp-java-server-install-dir  "/home/vravera/Application/Eclipse/lsp")
(after! (:and java-mode lsp)
  ;; (setq meghanada-server-install-dir "/home/vravera/Application/meghanada/")
  (require 'lsp-java)

  )
