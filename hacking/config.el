;;; hacking/config.el -*- lexical-binding: t; -*-

;; WIP don not source

;; from https://masteringemacs.org/article/comint-writing-command-interpreter

(defvar hack/revshell-executable "nc"
  "Reverse shell executable.")

(defvar hack/revshell-parameters '("-l" "-p")
  "Reverse shell arguments.")

(defvar hack/revshell-mode-map (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
                                 (define-key map "\t" 'completion-at-point)
                                 map)
  "Reverse shell basic kaymap.")

(defvar hack/revshell-prompt-regexp "^\\(?:\\[[^@]+@[^@]+\\]\\)"
   "Reverse shell possible prompt.")

(defun hack/revshell (port)
  "Run an reversell listener on PORT inside emacs."
  (interactive "nPort Number: ")
  (let* ((revshell-program hack/revshell-executable)
         (revshell-parameters (append hack/revshell-parameters (list (int-to-string port))))
         (buffer (comint-check-proc "revshell")))
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'revshell-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*revshell*"))
       (current-buffer)))
    (unless buffer
      (apply 'make-comint-in-buffer "revshell" buffer
             revshell-program nil revshell-parameters)
      (revshell-mode))))

(defun hack/revshell--initialize ()
  "Helper function to start a reverseshell."
  (setq-local comint-process-echoes t)
  (setq-local comint-use-prompt-regexp t))

(define-derived-mode revshell-mode comint-mode "revshell"
  "Major mode for `hack/revshell'.

\\<hack/revshell-mode-map>"
  nil "revshell"
  (setq comint-prompt-regexp hack/revshell-prompt-regexp)
  (setq comint-prompt-read-only t)
  (set (make-local-variable 'paragraph-separate) "\\'")
  ;; (set (make-local-variable 'font-lock-defaults) "\\'")
  (set (make-local-variable 'paragraph-start) hack/revshell-prompt-regexp))

(add-hook 'revshell-mode-hook 'hack/revshell--initialize)
