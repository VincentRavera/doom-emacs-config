;;; edifact/od-edi.el -*- lexical-binding: t; -*-


;;; Code:

(require 'ob)
(require 'seq)
(require 's)
(require 'edi-mode)

(defvar org-babel-default-header-args:edi
  '((:results . "code"))
  "Default arguments for evaluating an edi source block.

The evaluation returns the formmatted edi.")

(add-to-list 'org-src-lang-modes '("edi" . edi))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-babel interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-babel-execute:edi (body params)
  "Execute edi BODY but PARAMS are ignored."
  (message (format "ediparams: %s" params))
  (with-temp-buffer
    (insert body)
    (edi-edi-to-readable)
    (buffer-string)))
