;;; term/config.el -*- lexical-binding: t; -*-

(after! term
  (cond ((and (getenv "SHELL") (s-contains-p "bash" (getenv "SHELL")))
         (setq explicit-shell-file-name "bash"))
        ((and (getenv "SHELL") (s-contains-p "zsh" (getenv "SHELL")))
         (setq explicit-shell-file-name "zsh")
         )
  )
  ;; Binds C-c C-p and C-c C-n in normal mode in terminals
  ;; Goes to the previous/next commands inputed by the user
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>▶] *")
)
