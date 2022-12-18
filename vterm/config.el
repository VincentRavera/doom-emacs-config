;;; vterm/config.el -*- lexical-binding: t; -*-

(after! vterm
  (defun me/vterm-load-config ()
    "Pass local configuration files to vterm.

Allows remote vterm to be shell-side configured,
without altering remote config."
    (interactive)
    (let (;; Bellow a message to reassure other users that look at history
          ;; The hosts is not compromised (not by me), I am just trying to configure emacs
          (reasuring-message (format "Configuring shell of user %s to be emacs comptible"
                                     user-full-name))
          (reasuring-notice "This action is shell local, it will not affect other shells")
          ;; Bellow lies my configuration
          (basic-func-script (f-read-text (concat (getenv "HOME")
                                                  "/.doom.d/shells/sources/functions.sh")))
          ;; Bellow lies the vterm shell-side configuration
          ;; Must be sourced last
          (vterm-func-script (f-read-text (concat
                                           (file-name-directory (find-library-name "vterm"))
                                           "/etc/emacs-vterm-bash.sh"))))
      (vterm-insert (format "# START: %s\n" reasuring-message))
      (vterm-insert (format "# %s\n" reasuring-notice))
      ;; Create one single block in history
      (vterm-insert "{\n")
      (vterm-insert basic-func-script)
      (vterm-insert vterm-func-script)
      (vterm-insert "}\n")
      ;; End the single block in history
      (vterm-insert (format "# %s\n" reasuring-notice))
      (vterm-insert (format "# STOP: %s\n" reasuring-message))
      )
    )
  (defun me/--find-file-other-window-wrapper (file)
    "Help vterm find a FILE."
    (if (s-starts-with-p "/" file)
        (if (s-starts-with-p "/sudo::" file)
            ;; remove /sudo:: from file
            (find-file-other-window
             (doom--sudo-file-path
              (concat (file-remote-p default-directory)
                      (substring-no-properties file 7))))
          (find-file-other-window (concat (file-remote-p default-directory) file)))
      (find-file-other-window file)))
  ;; Allow ffow to me called from vterm
  (add-to-list 'vterm-eval-cmds '("find-file-other-window"
                                  me/--find-file-other-window-wrapper))
  )

;; (defun me/vterm-load-file ()
;;   (interactive)
;;   (let ((payload (f-read-text (concat (getenv "HOME") "/.doom.d/shells/sources/functions.sh") )))
;;     (vterm-insert payload)
;;     )
;;   )
;; (defun me/vterm-payload ()
;;   "Parse the vterm shell-side configuration oneliner and source it."
;;   (interactive)
;;   (let ((payload (f-read-text (concat (getenv "HOME") "/.doom.d/vterm/payload.sh") )))
;;     (vterm-insert payload)
;;     )
;;   )
