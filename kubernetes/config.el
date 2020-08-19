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
        :desc "statefulsets" "p" #'kubernetes-statefulsets-refresh-now
        )
       (:prefix ("d" . "display") ;; Display
        :desc "ingress" "i" #'kubernetes-display-ingress
        :desc "configmaps" "c" #'kubernetes-display-configmap
        :desc "jobs" "c" #'kubernetes-display-job
        :desc "node" "n" #'kubernetes-display-node
        :desc "services" "s" #'kubernetes-display-service
        :desc "Secret" "S" #'kubernetes-display-secret
        )
       :desc "switch contexts" "s" #'kubernetes-use-context
       ))
