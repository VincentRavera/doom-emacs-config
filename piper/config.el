;;; ../MY-DOOM/piper/config.el -*- lexical-binding: t; -*-
;;
;; Piper Integration
;;
;; Official Package: https://gitlab.com/howardabrams/emacs-piper.git
;;
;; Ideas:
;; - Minibuffers such as lsp edit template or yassnippet
;;      - Create a minibuffer with a pscript inited to edit the current buffer
;;
(use-package! piper
  :load-path "~/workspace/elisp/emacs-piper")

(map! :leader
      (:prefix ("|" . "Pipe") ;; Piper Tool
       :desc "Local" "|" #'piper
       :desc "Directories" "d" #'piper-other
       :desc "Remote" "r" #'piper-remote
       :desc "Buffer" "b" #'piper-user-interface
       ;; :desc "Scripting Buffer" "s" #'piper-pop-up-script-buffer
       ))
