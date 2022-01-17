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
