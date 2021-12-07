;; Org Configuration file
;; GLOBALS need for lazy
;; ORG - ROOT
(setq org-directory "~/Documents/ORG"
      ;; GLOBAL NOTES
      me/org-note-root  (concat org-directory "/NOTES")
      ;; GLOBAL AGENDA
      me/org-agenda-root (concat org-directory "/AGENDA")
      ;; DEFT
      deft-directory org-directory
      deft-recursive t
      ;; ROAM
      org-roam-directory me/org-note-root
      )
;; ORG - Files
(setq +org-capture-notes-file (concat me/org-note-root "/Notes.org")
      +org-capture-work-notes-file (concat me/org-note-root "/Work/WorkNotes.org")

      +org-capture-todo-file (concat me/org-agenda-root "/Todo.org")
      +org-capture-work-todo-file (concat me/org-agenda-root "/Work/WorkTodo.org")
      org-agenda-files
      (list +org-capture-todo-file                                    ;; For Active Tasks
            (eval (concat me/org-agenda-root "/Archive.org"))          ;; For Older Tasks
            +org-capture-work-todo-file                               ;; For Active Tasks
            (eval (concat me/org-agenda-root "/Work/WorkArchive.org"))     ;; For Older Tasks
            )

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
;; END GLOBAL

;; CAPTURE
(after! org-capture
  (setq org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)

          ("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n%i\n%a" :prepend t)

          ;; Work Templates
          ("w" "Templates for work")
          ("wt" "Project-local todo" entry
           (file+headline +org-capture-work-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)
          ("wn" "Project-local notes" entry
           (file+headline +org-capture-work-notes-file "Inbox")
           "* %U %?\n%i\n%a" :prepend t)

          ;; In Projects
          ("p" "Templates for projects")
          ("pt" "Project-local todo" entry
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)
          ("pn" "Project-local notes" entry
           (file+headline +org-capture-project-notes-file "Inbox")
           "* %U %?\n%i\n%a" :prepend t)
          ("pc" "Project-local changelog" entry
           (file+headline +org-capture-project-changelog-file "Unreleased")
           "* %U %?\n%i\n%a" :prepend t)

          ;; Centalized
          ("o" "Centralized templates for projects")
          ("ot" "Project todo" entry
           #'+org-capture-central-project-todo-file
           "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
          ("on" "Project notes" entry
           #'+org-capture-central-project-notes-file
           "* %U %?\n %i\n %a" :heading "Notes" :prepend nil)
          ("oc" "Project changelog" entry
           #'+org-capture-central-project-changelog-file
           "* %U %?\n %i\n %a" :heading "Changelog" :prepend nil))
        )
  )

(after! org-roam
  (setq org-roam-capture-templates
        `(("d" "default" plain "%?"
           :target (file+head "${slug}.org" "#+title: ${title}\n#filetags: ")
           :unnarrowed t)))
  )

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


  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (setq org-tag-alist
        '((:startgroup)
                                        ; Put mutually exclusive tags here
          (:endgroup)
          ("@task" . ?E)
          ("@work" . ?W)
          ("@idea" . ?i)))


  ;; VM MANGEMENT
  (defun org-ssh-connect (&optional arg)
    "Connect to the host at point and open `eshell'.
If ARG is non-nil, open `dired' instead of `eshell'."
    (interactive "P")
    (let* ((properties (org-entry-properties))
           (message properties)
           (name (alist-get "ITEM" properties nil nil #'string=))
           (user (alist-get "SSH_USER" properties nil nil #'string=))
           (port (alist-get "SSH_PORT" properties nil nil #'string=))
           (sudo (alist-get "SSH_SUDO" properties nil nil #'string=))
           (path (alist-get "SSH_PATH" properties nil nil #'string=))
           (host (or (alist-get "IP" properties nil nil #'string=)
                     (alist-get "HOSTNAME" properties nil nil #'string=))))
      (if host
          (let ((default-directory (format "/ssh:%s%s%s%s:%s"
                                           (if user (format "%s@" user) "")
                                           host
                                           (if sudo (format "|sudo:root@%s" host) "")
                                           (if port (format "#%s" port) "")
                                           (if path path ""))))
            (message "Connecting to %s..." name)
            (if arg
                (eshell t)
              (dired ".")))
        (user-error "Not an SSH host"))))



  )

;; Agenda
(after! org-agenda
  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "INPROGRESS"
                  ((org-agenda-overriding-header "Doing now")))
            (todo "TODO"
                  ((org-agenda-overriding-header "Next Tasks")))

            (todo "BLOCKED"
                  ((org-agenda-overriding-header "Blocked by circumstences")))
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
          )
        )
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
