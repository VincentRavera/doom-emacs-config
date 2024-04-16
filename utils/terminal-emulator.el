;;; utils/terminal-emulator.el -*- lexical-binding: t; -*-

(defvar me/term-workspace "main"
  "The terminal workspace name.")

(defun me/term-workspace ()
  "Get or create the terminal workspace."
  (when (not (+workspace-exists-p me/term-workspace))
    (+workspace-new me/term-workspace))
  (+workspace-switch me/term-workspace))

(defun me/terminal-emulator-vterms ()
  "Spawn a Vterm in it's session."
  (interactive)
  ;; Create Frame
  ;; GOTO Workspace
  ;; Set default Directory to HOME
  ;; Create terminal buffer
  ;; link lives of buffer and frame
  (let ((term-frame (make-frame)))
    (with-selected-frame term-frame
      (me/term-workspace)
      (let* ((default-directory "~/")
             (term-buffer (+vterm/here t))
             )
        (with-current-buffer term-buffer
          (+workspaces-add-current-buffer-h)
          ;; (add-hook 'kill-buffer-hook
          ;;           (lambda ()
          ;;             (when (eq (current) term-buffer)
          ;;               (delete-frame term-frame))))
          )))))

(defun me/terminal-emulator-eshell ()
  "Spawn a Eshell in it's session."
  (interactive)
  ;; Create Frame
  ;; GOTO Workspace
  ;; Set default Directory to HOME
  ;; Create terminal buffer
  ;; link lives of buffer and frame
  (let ((term-frame (make-frame)))
    (with-selected-frame term-frame
      (me/term-workspace)
      (let* ((default-directory "~/")
             (term-buffer (+eshell/here))
             )
        (with-current-buffer term-buffer
          (+workspaces-add-current-buffer-h)
          ;; (add-hook 'kill-buffer-hook
          ;;           (lambda ()
          ;;             (when (eq (current) term-buffer)
          ;;               (delete-frame term-frame))))
          )))))
