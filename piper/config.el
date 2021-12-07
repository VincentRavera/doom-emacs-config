;;; ../MY-DOOM/piper/config.el -*- lexical-binding: t; -*-
;;
;; Piper Integration
;;
;; Official Package: https://gitlab.com/howardabrams/emacs-piper.git
;;
;;
(use-package! piper
  :load-path "~/.emacs.d/.local/straight/repos/emacs-piper")

(setq piper-db-path "~/workspace/perso/piperdb/")

(defun me/piper-db-explore ()
  (interactive)
  (->> (piper-script
        (shell (format "find %s -type f" (file-truename piper-db-path)))
        (grep ".el$")
        (sed ".el$" "")
        (sed (file-truename piper-db-path) ""))
       (split-string)
       (completing-read "Script :")
       (format "%s%s.el" piper-db-path)
       (find-file)))

(defun me/piper-db-execute ()
  (interactive)
  (->> (piper-script
        (shell (format "find %s -type f" (file-truename piper-db-path)))
        (grep ".el$")
        (sed ".el$" "")
        (sed (file-truename piper-db-path) ""))
       (split-string)
       (completing-read "Script :")
       (format "%s%s.el" piper-db-path)
       (load-file)))

(defun me/piper-db-save ()
  (interactive)
  (->> (piper-script
        (shell (format "find %s -type f" (file-truename piper-db-path)))
        (grep ".el$")
        (sed ".el$" "")
        (sed (file-truename piper-db-path) ""))
       (split-string)
       (completing-read "Script :")
       (format "%s%s.el" piper-db-path)
       (write-file)))


(map! :leader
      (:prefix ("|" . "Pipe") ;; Piper Tool
       :desc "Locally" "|" #'piper
       :desc "Project" "p" #'piper-project
       :desc "Directories" "d" #'piper-other
       :desc "Remote" "r" #'piper-remote
       :desc "On current Buffer" "b" #'piper-user-interface
       :desc "Open piper-scripting Buffer" "s" #'piper-popen-piper-script
       :desc "Explore saved scripts" "f" #'me/piper-db-explore
       :desc "Execute saved scripts" "x" #'me/piper-db-execute
       :desc "Save script to database" "w" #'me/piper-db-save
       ))
