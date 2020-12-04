;; Org Configuration file
(after! org
  (setq org-ellipsis " ▾")
  ;; TODOS
  (setq org-todo-keywords
        '((sequence "TODO(t!)"
                    "INPROGRESS(i!)"
                    "BLOCKED(b!)"
                    "|"
                    "HACKED(h!)"
                    "CANCELLED(c!)"
                    "DONE(d!)"))
        org-todo-keyword-faces
        '(("TODO" . org-warning)
          ("INPROGRESS" . "#E35DBF")
          ("CANCELED" . (:foreground "white" :background "#4d4d4d" :weight bold))
          ("HACKED" . (:foreground "green" :background "black" :weight italic))
          ("BLOCKED" . "pink")))
  (setq org-log-into-drawer t)

  (setq org-agenda-files
        '( "~/Documents/ORG-ROAM/Work-data/todo.org"
           "~/Documents/ORG-ROAM/Work-data/Archive.org"
           "~/Documents/ORG-ROAM/Work-data/Livraisons.org")
        ;; org-agenda-text-search-extra-files
        ;; '(agenda-archives
        ;;   "~/workspace/wiki/VRA-Notes/2020/Notes-2020.org"
        ;;   "~/workspace/wiki/VRA-Notes/2019/DT4.org"
        ;;   "~/workspace/wiki/VRA-Notes/2020/2020-01.org"
        ;;   "~/workspace/wiki/VRA-Notes/2020/2020-02.org"
        ;;   "~/workspace/wiki/VRA-Notes/2020/2020-03.org"
        ;;   "~/workspace/wiki/VRA-Notes/2020/2020-04.org"
        ;;   "~/workspace/wiki/VRA-Notes/2020/2020-05.org"
        ;; )
        )
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
  (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
  (setq org-refile-use-outline-path t)                  ; Show full paths for refiling
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (setq org-tag-alist
        '((:startgroup)
                                        ; Put mutually exclusive tags here
          (:endgroup)
          ("@task" . ?E)
          ("@work" . ?W)
          ("@idea" . ?i)))
  )

;; Agenda
(after! org-agenda
  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "TODO"
                  ((org-agenda-overriding-header "Next Tasks")))

            (todo "INPROGRESS"
                  ((org-agenda-overriding-header "Doing now")))
            (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))
          ("w" "Workflow Status"
           ((todo "TODO"
                  ((org-agenda-overriding-header "Waiting")
                   (org-agenda-files org-agenda-files)))
            (todo "INPROGRESS"
                  ((org-agenda-overriding-header "Doing now")
                   (org-agenda-files org-agenda-files)))
            (todo "BLOCKED"
                  ((org-agenda-overriding-header "Blocked by circumstences")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "DONE"
                  ((org-agenda-overriding-header "Completed")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "HACKED"
                  ((org-agenda-overriding-header "Completed unexpectedly")
                   (org-agenda-files org-agenda-files)))
            (todo "CANCELED"
                  ((org-agenda-overriding-header "Cancelled")
                   (org-agenda-files org-agenda-files)))
            ))
          ))
  )

;; Journal
;; (after! org-journal
;;   (setq org-enable-org-journal-support t)
;;   (setq org-journal-dir "~/workspace/wiki/VRA-Notes/2020/"
;;         org-journal-file-format "%Y-%m.org"
;;         org-journal-date-prefix "* "
;;         org-journal-date-format "%Y/%m/%d"
;;         org-journal-time-prefix "** "
;;         org-journal-time-format "%Hh%Mm: "))

(load! "org-keybindings.el")
