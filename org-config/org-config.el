;; Org Configuration file
(after! org
  ;; TODOS
  (setq org-todo-keywords
        '((sequence "TODO(t!)"
                    "DOINGNOW(d!)"
                    "BLOCKED(b!)"
                    "|"
                    "HACKED(h!)"
                    "CANCELLED(c!)"
                    "DONE(F!)"))
        org-todo-keyword-faces
        '(("TODO" . org-warning)
          ("DOINGNOW" . "#E35DBF")
          ("CANCELED" . (:foreground "white" :background "#4d4d4d" :weight bold))
          ("HACKED" . (:foreground "green" :background "black" :weight italic))
          ("BLOCKED" . "pink"))))

;; Agenda
(after! org-agenda
  (setq org-agenda-files '("~/workspace/wiki/VRA-Notes/2020")
        org-agenda-text-search-extra-files
        '(agenda-archives
          "~/workspace/wiki/VRA-Notes/2020/Notes-2020.org"
          "~/workspace/wiki/VRA-Notes/2019/DT4.org")))

;; Journal
(after! org-journal
  (setq org-enable-org-journal-support t
        org-journal-dir "~/workspace/wiki/VRA-Notes/2020/"
        org-journal-file-format "%Y-%m.org"
        org-journal-date-prefix "* "
        org-journal-date-format "2020/%m/%d"
        org-journal-time-prefix "** "
        org-journal-time-format "%H:%M - Task: "))

(load! "org-keybindings.el")
