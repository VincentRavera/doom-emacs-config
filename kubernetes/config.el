;;; $DOOMDIR/kubernetes/config.el -*- lexical-binding: t; -*-

(use-package! kubernetes-evil
  :after-call kubernetes-overview-mode-hook
  )

(map! :leader
      (:prefix ("k" . "kubernetes") ;; Kubernetes support
       :desc "Overview" "k" #'kubernetes-overview
       (:prefix ("r" . "refresh") ;; Refresh
        :desc "all" "r" #'kubernetes-refresh
        :desc "deploys" "d" #'kubernetes-deployments-refresh-now
        :desc "pods" "p" #'kubernetes-pods-refresh-now
        :desc "statefulsets" "s" #'kubernetes-statefulsets-refresh-now
        )
       (:prefix ("d" . "display") ;; Display
        :desc "ingress" "i" #'kubernetes-display-ingress
        :desc "pod" "p" #'kubernetes-display-pod
        :desc "configmaps" "c" #'kubernetes-display-configmap
        :desc "jobs" "c" #'kubernetes-display-job
        :desc "node" "n" #'kubernetes-display-node
        :desc "services" "s" #'kubernetes-display-service
        :desc "Secret" "S" #'kubernetes-display-secret
        )
       :desc "switch contexts" "s" #'kubernetes-contexts-use-context
       :desc "switch namespace" "n" #'kubernetes-set-namespace
       ))

;; (after! kubernetes-vars
;;   (setq kubernetes-overview-custom-views-alist '((replicasets . (context replicasets)))))
;;
(after! kubernetes
  (setq kubernetes-pod-buffer-name "*kubernetes pod: %s*"
        kubernetes-display-secret-buffer-name "*kubernetes secret: %s*"
        kubernetes-display-namespace-buffer-name "*kubernetes namespace: %s*"
        kubernetes-display-service-buffer-name "*kubernetes service: %s*"
        kubernetes-display-configmap-buffer-name "*kubernetes configmap: %s*"
        kubernetes-display-job-buffer-name "*kubernetes job: %s*"
        kubernetes-display-ingress-buffer-name "*kubernetes ingress: %s*"
        kubernetes-display-statefulset-buffer-name "*kubernetes statefulset: %s*"
        kubernetes-display-node-buffer-name "*kubernetes node: %s*"
        kubernetes-display-deployment-buffer-name "*kubernetes deployment: %s*")
  (defun kubernetes-yaml-make-buffer (bufname parsed-json)
    "[OVERRIDDEN] utils to create a yaml buffer, will format buffer name if possible."
    (let* ((betterbufname (format bufname
                               (->> parsed-json
                                    (alist-get 'metadata)
                                    (alist-get 'name))))
         (buf (get-buffer-create betterbufname)))
    (with-current-buffer buf
      (kubernetes-display-thing-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (kubernetes-ast-eval (kubernetes-yaml-render parsed-json))
        (goto-char (point-min))))
    buf)))
