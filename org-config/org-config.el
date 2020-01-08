;; Org Configuration file
(load! "org-keybindings.el")

;; TODOS
(setq org-todo-keywords
      '((sequence "TODO(t!)"
                  "DOINGNOW(d!)"
                  "BLOCKED(b!)"
                  "|"
                  "HACKED(h!)"
                  "CANCELLED(c!)"
                  "DONE(F!)")))
(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
        ("DOINGNOW" . "#E35DBF")
        ("CANCELED" . (:foreground "white" :background "#4d4d4d" :weight bold))
        ("HACKED" . (:foreground "green" :background "black" :weight italic))
        ("BLOCKED" . "pink")))

;; Agenda
(setq org-agenda-files '("~/workspace/wiki/VRA-Notes/2020"))
(setq org-agenda-text-search-extra-files
      '(agenda-archives
        "~/workspace/wiki/VRA-Notes/2020/Notes-2020.org"
        "~/workspace/wiki/VRA-Notes/2019/DT4.org"))
