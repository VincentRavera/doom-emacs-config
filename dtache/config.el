;;; dtache/config.el -*- lexical-binding: t; -*-

(setq dtache-db-directory (expand-file-name ".dtache/" "~/"))
(setq dtache-session-directory (expand-file-name "dtache" (temporary-file-directory)))
(setq dtache-shell-history-file (expand-file-name ".bash_history" "~/"))
(after! (:and embark dtache)
  (defvar embark-dtache-map (make-composed-keymap dtache-action-map embark-general-map))
  (add-to-list 'embark-keymap-alist '(dtache . embark-dtache-map)))

(map! :leader
      (:prefix ("d" . "dtache") ;; Dtache
       :desc "create" "c" #'dtache-shell-command
       :desc "Compare output" "C" #'dtache-diff-session
       :desc "delete" "d" #'dtache-delete-session
       :desc "kill" "k" #'dtache-kill-session
       :desc "list" "l" #'dtache-consult-session
       :desc "quit" "q" #'dtache-quit-tail-output
       :desc "rerun" "r" #'dtache-rerun-session))

;; Not working
(defun my/dtache-state-transition-alert-notification (session)
  "Send an `alert' notification when SESSION becomes inactive."
  (let ((status (car (dtache--session-status session)))
        (host (car (dtache--session-host session))))
    (alert (dtache--session-command session)
     :title (pcase status
              ('success (format "Dtache finished [%s]" host))
              ('failure (format "Dtache failed [%s]" host)))
     :severity (pcase status
                ('success 'moderate)
                ('failure 'high)))))

(defun my/dtache-alert-notification (session)
  (let* ((status (dtache--session-status session))
         (host (dtache--session-host session))
         (command (dtache--session-command session))
         (working-directory (dtache--session-working-directory session))
         (state (pcase status
                  ('success "Success")
                  ('failure "Failure")
                  (_ "???")))
         (level (pcase status
                  ('success "moderate")
                  (_ "critical"))))
    (shell-command (format "notify-send --urgency '%s' '%s at %s' '\n$ %s\n\nin %s'" level state host command working-directory))
    ))


(setq dtache-notification-function #'my/dtache-alert-notification)
