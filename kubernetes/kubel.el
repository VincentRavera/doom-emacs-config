;;; kubernetes/kubel.el -*- lexical-binding: t; -*-

(use-package! kubel-evil
  :after-call kubel-mode-hook)

(map! :leader
      (:prefix ("k" . "kubernetes") ;; Kubernetes support
       :desc "Overview" "k" #'kubel
       :desc "Refresh" "r" #'kubel-refresh
       :desc "get" "g" #'kubel-set-resource
       :desc "yaml" "y" #'kubel-get-resource-details
       :desc "Describe" "d" (lambda ()
                              (interactive)
                              (kubel-get-resource-details t)
                              (with-current-buffer (current-buffer)
                                  (goto-char (point-min)))

                              ;; (+popup/raise (current-buffer))
                              )
       :desc "switch contexts" "s" #'kubel-set-context
       :desc "switch namespace" "n" #'kubel-set-namespace
       ))
