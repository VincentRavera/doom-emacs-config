;;; edifact/config.el -*- lexical-binding: t; -*-

;; from https://www.emacswiki.org/emacs/download/edi-mode.el
;; Documentation https://www.emacswiki.org/emacs/EdiMode
(load! "./edi-mode.el")
(after! ob
  (load! "./od-edi.el"))
