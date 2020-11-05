;;; ../MY-DOOM/lsp/config.el -*- lexical-binding: t; -*-

(after! (:and company)
  (require 'company-lsp)
  (push 'company-lsp company-backends)
  )
