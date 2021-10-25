;;; tramp/config.el -*- lexical-binding: t; -*-
(after! tramp
  (defun me/tramp/clear ()
    "Shuts down Tramp"
    (interactive)
    (tramp-cleanup-all-connections)
    (tramp-cleanup-all-buffers))

  ;; Tramp on remote guix
  (setq tramp-remote-path
    (append
     '(tramp-own-remote-path)
     tramp-remote-path))
  )
