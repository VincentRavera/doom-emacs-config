;;; go-templates/config.el -*- lexical-binding: t; -*-
; https://gist.githubusercontent.com/grafov/10985431/raw/dcd12037308446179f26f7d2ab2c034a1e995d2e/go-template-mode.el

(load! "./go-template-mode.el")
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . go-template-mode))
(add-hook 'go-template-mode-hook 'display-line-numbers-mode)
