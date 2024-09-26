;;; utils/config.el -*- lexical-binding: t; -*-

;;; AUTOLOAD AND LOADS
;; Forced Evil
(autoload
  'me/forced-evil
  (file-name-concat doom-private-dir "utils/forced-evil.el")
  "evil binding in a transient state."
  t)

;; Terminal-emulator
;; (load! "./terminal-emulator.el")
(autoload
  'me/terminal-emulator-vterms
  (file-name-concat doom-private-dir "utils/terminal-emulator.el")
  "The terminal emulator config."
  t)
(autoload
  'me/terminal-emulator-eshell
  (file-name-concat doom-private-dir "utils/terminal-emulator.el")
  "The terminal emulator config."
  t)




;;; UTILITY FUNCTIONS
(defun me/get-filename-to-clipboard ()
  "Copy file path to clipboard."
  (interactive)
  (let ((absolute-path (buffer-file-name)))
    (kill-new absolute-path)
    (message absolute-path)))

;; Display ansi color in buffer
;; https://stackoverflow.com/questions/23378271/how-do-i-display-ansi-color-codes-in-emacs-for-any-mode
(defun me/display-ansi-colors ()
  "Print ansi color code in buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

;; Lisp list to Commandline
;; Must never depend on a module/package
(defun me/compact-list-to-sting (list-args)
  "Concatenate LIST-ARGS a list of string to a string seperated by spaces"
  (if list-args
      (concat (car list-args) " " (me/compact-list-to-sting (cdr list-args)))
    ""))

(defun me/startup ()
  (interactive)
  ;; Do I3 Integration setup
  (doom/load-session "~/.config/emacs/.local/etc/workspaces/Startup")
  (with-selected-frame (car (frame-list))
    (shell-command "i3-msg move container to workspace '2: '"))
  (let ((frame (make-frame)))
    (with-selected-frame frame
      (+workspace/switch-to-0)
      (switch-to-buffer (car (+workspace-buffer-list)))
      (shell-command "i3-msg move container to workspace '9: '")))
  (+workspace/switch-to-1)
  (switch-to-buffer (car (+workspace-buffer-list))))

;;; PATCHS
;; Dead circonflex
;; https://unix.stackexchange.com/questions/28170/some-keys-are-invalid-on-emacs-when-using-german-keyboard
(define-key key-translation-map [dead-circumflex] "^")

;; Buffer/Windows/Workspace management
; add current buffer to workspace
(map! :leader
      (:prefix-map ("b" . "buffer")
       (:prefix ("w" . "workspace")
        :desc "Add buffer to workspace" "a" #'persp-add-buffer
        :desc "Delete buffer from workspace" "d" #'persp-remove-buffer))
      (:prefix-map ("TAB" . "workspace")
       :desc "Add buffer to workspace" "a" #'persp-add-buffer
       :desc "Delete buffer from workspace" "D" #'persp-remove-buffer
       :desc "Switch Workspace" "SPC" #'+workspace/switch-to))
