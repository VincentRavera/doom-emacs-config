;;; ../MY-DOOM/piper/config.el -*- lexical-binding: t; -*-
;;
;; Piper Integration
;;
;; Official Package: https://gitlab.com/howardabrams/emacs-piper.git
;;
;;
(use-package! piper
  :load-path "~/.emacs.d/.local/straight/repos/emacs-piper")

(map! :leader
      (:prefix ("|" . "Pipe") ;; Piper Tool
       :desc "Local" "|" #'piper
       :desc "Directories" "d" #'piper-other
       :desc "Remote" "r" #'piper-remote
       :desc "Buffer" "b" #'piper-user-interface
       :desc "Scripting Buffer" "p" #'piper-popen-piper-script
       ))

(map! :leader
      (:prefix ("a" . "Pipe") ;; Piper Tool
       :desc "Local" "a" #'piper
       :desc "Directories" "d" #'piper-other
       :desc "Remote" "r" #'piper-remote
       :desc "Buffer" "b" #'piper-user-interface
       :desc "Scripting Buffer" "p" #'piper-popen-piper-script
       ))
