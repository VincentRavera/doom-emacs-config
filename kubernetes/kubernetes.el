;;; $DOOMDIR/kubernetes/config.el -*- lexical-binding: t; -*-

(use-package! kubernetes-evil
  :after-call kubernetes-overview-mode-hook
  )

;; Preload funtion to be able to call it from bindings
(autoload
  'me/kubernetes-display-guess
  (concat doom-private-dir "../kubernetes/kubernetes-utils.el")
  "Additional function for k8s." t)

(map! :leader
      (:prefix ("k" . "kubernetes") ;; Kubernetes support
       :desc "Overview" "k" #'kubernetes-overview
       (:prefix ("r" . "refresh") ;; Refresh
        :desc "all" "r" #'kubernetes-refresh
        :desc "deploys" "d" #'kubernetes-deployments-refresh-now
        :desc "pods" "p" #'kubernetes-pods-refresh-now
        :desc "statefulsets" "s" #'kubernetes-statefulsets-refresh-now
        )
       (:prefix ("g" . "Get")
        :desc "configmaps" "c" #'kubernetes-display-configmap
        :desc "ingress" "i" #'kubernetes-display-ingress
        :desc "jobs" "j" #'kubernetes-display-job
        :desc "guess" "g" #'me/kubernetes-display-guess
        :desc "node" "n" #'kubernetes-display-node
        :desc "pod" "p" #'kubernetes-display-pod
        :desc "pvC" "C" #'kubernetes-display-persistentvolumeclaim
        :desc "services" "s" #'kubernetes-display-service
        :desc "Secret" "S" #'kubernetes-display-secret
        )
       (:prefix ("d" . "describe")
        :desc "pod" "p" #'kubernetes-describe-pod
        )
       :desc "switch contexts" "s" #'kubernetes-contexts-use-context
       :desc "switch namespace" "n" #'kubernetes-set-namespace
       ))

;; (after! kubernetes-vars
;;   (setq kubernetes-overview-custom-views-alist '((replicasets . (context replicasets)))))
;;
(after! kubernetes

  ;; Performence issues
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600)

  ;; Load additional object implementations
  ;; (load! "./extension/kubernetes-persistentvolume.el")

  ;; Buffer Naming
  ;; Allow multiple frames of K8S
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
  (load! "./kubernetes-utils.el"))
