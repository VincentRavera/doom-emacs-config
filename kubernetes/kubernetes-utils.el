;;; kubernetes/utils.el -*- lexical-binding: t; -*-

;; Overides
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
      buf))

;; Variables
(defvar kubernetes-display-get-object-map
  '((ConfigMaps . kubernetes-display-configmap)
    (cm . kubernetes-display-configmap)

    (pvc                      . kubernetes-display-persistentvolumeclaim)
    (PersistentVolumeClaims   . kubernetes-display-persistentvolumeclaim)

    (Ingress  . kubernetes-display-ingress)
    (Jobs     . kubernetes-display-job)
    (Node     . kubernetes-display-node)
    (Pods     . kubernetes-display-pod)
    (Services . kubernetes-display-service)
    (Secrets  . kubernetes-display-secret))
  "Map between k8s resource and display function.")

;; Functions
(defun me/kubernetes-display-guess (k8s-resource)
    "Ask what you kubernetes object you want to display."
    (interactive
     (list (completing-read "kubectl get " (mapcar 'car kubernetes-display-get-object-map))))
    (call-interactively
     (cdr
      (assq (intern k8s-resource) kubernetes-display-get-object-map))))
