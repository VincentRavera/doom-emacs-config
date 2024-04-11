;;; vterm/config.el -*- lexical-binding: t; -*-

(after! vterm
  ;;;; Add bash to default vterm shell
  ;; Warning if bash is not present in the remote this may cause errors
  ;;; How to know what to add:
  ;; go to buffer where you will start vterm (tramp buffer)
  ;; run:
  ;; (list (plist-get (connection-local-criteria-for-default-directory) :protocol) "/bin/bash")
  ;; This is the list you will need to add to vterm-tramp-shells
  ;; to force bash instead of /bin/sh or other
  (add-to-list 'vterm-tramp-shells '("ssh" "/bin/bash"))
  (add-to-list 'vterm-tramp-shells '("sudo" "/bin/bash"))
  (add-to-list 'vterm-tramp-shells '("vagrant" "/bin/bash"))
  ;;; Vterm overides
  ;; Original functions overwrites tramp path with a guessed path.
  ;; However it breaks if remote fqdn/hostname is not resolbale by local machine
  ;; could also break on port forwarding, multihops,
  ;; custom protocol such as: docker, vagrant, ...
  ;; *if* you try to shell-side configure them.
  ;; Easily testable with vagrant ssh port on localhost.
  ;; My workflow is to open a tramp dired on / of the remote to get a
  ;; "foothold" then open vterms from there.
  (defun vterm--get-directory (path)
    "[OVERLOADED] Get normalized directory to PATH."
    (when path
      (let (directory)
        (if (string-match "^\\(.*?\\)@\\(.*?\\):\\(.*?\\)$" path)
            (progn
              (let ((user (match-string 1 path))
                    (host (match-string 2 path))
                    (dir (match-string 3 path)))
                (if (and (string-equal user user-login-name)
                         (string-equal host (system-name)))
                    (progn
                      (when (file-directory-p dir)
                        (setq directory (file-name-as-directory dir))))
                  (setq directory
                        ;; Bellow is what i altered
                        (file-name-as-directory (concat (file-remote-p default-directory) dir))))))
          (when (file-directory-p path)
            (setq directory (file-name-as-directory path))))
        directory)))
  ;; Injects the payload to the vterm buffer.
  (defun me/vterm-load-config ()
    "Pass local configuration files to vterm.

Allows remote vterm to be shell-side configured,
without altering remote config.
Also adds my personal configuration that does not rely
too much on external packages.
Prints a reasuring message to proove good faith."
    (interactive)
    (let (;; Bellow messages to reassure other users that look at history
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

  (defun me/vterm-paste-proxy ()
    "Fetch proxy variables and paste it"
    (interactive)
    (let ((proxy-envs '("HTTP_PROXY"
                        "HTTPS_PROXY"
                        "NO_PROXY"
                        "http_proxy"
                        "https_proxy"
                        "no_proxy")))

      (vterm-insert
       (--reduce-from (format "%s%s=\"%s\" " acc it (getenv it)) "" proxy-envs))
      ))

  ;; find-file-other-window does not works great on remote:
  ;; if given an absolute path on a remote host,
  ;; the path will be understood as a local file since no
  ;; tramp prefix is present, and bash does not care
  ;; about tramp prefixes.
  ;; Bellow we solve context before sending it to
  ;; ffow
  (defun me/vterm--find-file-other-window-wrapper (file)
    "Help vterm find a FILE."
    (find-file-other-window (me/vterm--ffow-resolver file)))
  (defun me/vterm--ffow-resolver (file)
    "Help vterm resolve FILE."
    (cond
     ;; "/sudo::"
     ;; doom--sudo-file-path does the trick for us
     ((s-starts-with-p "/sudo::" file)
      (doom--sudo-file-path
       (concat (file-remote-p default-directory)
               (substring-no-properties file 7))))
     ;; "/" means we want the "Relative root"
     ;; try appending the remote prefix if relevent
     ((s-starts-with-p "/" file)
      (concat (file-remote-p default-directory) file))
     ;; we got a relative path
     ;; we don't need to help ffow to find it
     (t
      file)))

  ;; The variable vterm-eval-cmds is a SERIOUSLY SENSIBLE variable !
  ;; Do not be the guy that adds RCE into their config !

  ;; Allow customed ffow to be called from vterm
  ;; ffow should be as safe as find-file which is already trusted
  ;; we append our resolver that only manipulate strings,
  ;; Proove me wrong but i think it's safe.
  (add-to-list 'vterm-eval-cmds '("find-file-other-window"
                                  me/vterm--find-file-other-window-wrapper)))

(setq me/vterm-workspace "*Terms*")
(defun me/terminal-emulator-vterms ()
  "Spawn a Vterm in it's session."
  (interactive)

  ;; Create ws if needed
  (with-selected-frame (make-frame)
    (when (not (+workspace-exists-p me/vterm-workspace))
      (+workspace-new me/vterm-workspace))
    ;; switch to ws
    (+workspace-switch me/vterm-workspace)
    (let ((default-directory "~/"))
      (+vterm/here t))))

(defun me/terminal-emulator-eshell ()
  "Spawn a Eshell in it's session."
  (interactive)
  ;; Create ws if needed
  (with-selected-frame (make-frame)
    (when (not (+workspace-exists-p me/vterm-workspace))
      (+workspace-new me/vterm-workspace))
    ;; switch to ws
    (+workspace-switch me/vterm-workspace)
    (let ((default-directory "~/"))
      (+eshell/here))))
