;;; browser/firefox.el -*- lexical-binding: t; -*-

(setq browse-url-librewolf-program "librewolf")

(defun browse-url-librewolf (url &optional new-window)
  "Ask the Librewolf WWW browser to load URL.
Defaults to the URL around or before point.  Passes the strings
in the variable `browse-url-firefox-arguments' to Librewolf.

Interactively, if the variable `browse-url-new-window-flag' is non-nil,
loads the document in a new Librewolf window.  A non-nil prefix argument
reverses the effect of `browse-url-new-window-flag'.

If `browse-url-firefox-new-window-is-tab' is non-nil, then
whenever a document would otherwise be loaded in a new window, it
is loaded in a new tab in an existing window instead.

Non-interactively, this uses the optional second argument NEW-WINDOW
instead of `browse-url-new-window-flag'."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply #'start-process
           (concat "librewolf " url) nil
           browse-url-librewolf-program
           (append
            browse-url-firefox-arguments
            (if (browse-url-maybe-new-window new-window)
		(if browse-url-firefox-new-window-is-tab
		    '("-new-tab")
		  '("-new-window")))
            (list url)))))

;; Set Librewolf as the default
(setq browse-url-browser-function 'browse-url-librewolf
      browse-url-firefox-new-window-is-tab t)
