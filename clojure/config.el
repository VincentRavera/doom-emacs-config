;;; clojure/config.el -*- lexical-binding: t; -*-

;; https://emacs-lsp.github.io/lsp-mode/tutorials/clojure-guide/
(use-package! cider
  :after clojure-mode
  :config
  (set-lookup-handlers! 'cider-mode nil))

(use-package! clj-refactor
  :after clojure-mode
  :config
  (set-lookup-handlers! 'clj-refactor-mode nil))
