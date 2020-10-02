;;; java/config.el -*- lexical-binding: t; -*-

(after! (:and java-mode lsp)
  (setq lsp-java-server-install-dir  "/home/m0b1us/Applications/lsp/java/")
  (require 'lsp-java)
  )
