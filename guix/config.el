;;; guix/config.el -*- lexical-binding: t; -*-

(defun guix-load-profile ()
  "Load a profile from list `guix-profiles' and not a path"
  (interactive)
  (when (not (bound-and-true-p guix-profiles))
    (require 'guix-ui-profile)
    (guix-all-profiles))
  (guix-set-emacs-environment (completing-read "Profile: " (guix-all-profiles))))

(map! :leader
      (:prefix ("G" . "Guix")
       :desc "Load profile" "p" #'guix-load-profile
       :desc "Guix UI" "G" #'guix))
