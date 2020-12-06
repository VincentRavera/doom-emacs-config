;;; ~/.doom.d/eshell/config.el -*- lexical-binding: t; -*-
;;; ESHELL CONFIGURATION
;; Eshell Environ
(setenv "EDITOR" "emacsclient")
;; (defvar eshell-visual-commands nil)
;; (defvar eshell-visual-options nil)
;; (defvar eshell-visual-subcommands nil)
(after! (:and eshell em-term)
  (add-to-list 'eshell-visual-commands          "top")
  (add-to-list 'eshell-visual-options           '("git" "--help"))
  (add-to-list 'eshell-visual-subcommands       '("git" "log" "diff" "show"))

  (setq eshell-destroy-buffer-when-process-dies         nil
        eshell-history-size                             10000
        eshell-buffer-maximum-lines                     10000
        eshell-hist-ignoredups                          t
        eshell-scroll-to-bottom-on-input                t)

  (defun me/eshell/remote-find-file (input-file-name)
    "open remotly the absolut file you want"
    (let ((prefix-tramp (file-remote-p (eshell/pwd))))
      (find-file (concat prefix-tramp input-file-name))))

  (defun me/eshell/remote-change-directory (input-dir-name)
    "cd remotly the aboslute path folder you want"
    (let ((prefix-tramp (file-remote-p (eshell/pwd))))
      (eshell/cd (concat prefix-tramp input-dir-name))))

  (defun me/eshell/insert-remote-prefix ()
    "Insert the remote prefix at point"
    (interactive)
    (insert (me/eshell/get-remote-prefix)))

  (defun me/eshell/get-remote-prefix ()
    "outputs the remote prefix"
    (file-remote-p (eshell/pwd)))

  (add-hook! 'eshell-first-time-mode-hook
    (defun +me/eshell/hook-binding ()
      ;; add personal keybindings for eshell
      (map! :map eshell-mode-map
            "C-c r"   #'me/eshell/insert-remote-prefix)
      (map! :map eshell-mode-map
            "C-c C-r"   #'counsel-esh-history)
      )
    )
  )


;; get TRAMP prefix
;; (tramp-handle-file-remote-p $PWD) => /sudo:root@localhost:
