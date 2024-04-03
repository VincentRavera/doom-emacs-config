;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; This is where you install packages, by declaring them with the `package!'
;; macro, then running 'doom refresh' on the command line. You'll need to
;; restart Emacs for your changes to take effect! Or at least, run M-x
;; `doom/reload'.
;;
;; WARNING: Don't disable core packages listed in ~/.emacs.d/core/packages.el.
;; Doom requires these, and disabling them may have terrible side effects.
;;
;; Here are a couple examples:


;; All of Doom's packages are pinned to a specific commit, and updated from
;; release to release. To un-pin all packages and live on the edge, do:
;(setq doom-pinned-packages nil)

;; ...but to unpin a single package:
;(package! pinned-package :pin nil)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))
(package! lsp-ui)
;; (package! lsp-docker)
;; (package! pyenv-mode)

;; Carfull buggy, but pretty
;; (package! kubernetes)
;; (package! kubernetes-evil)
;; Stable and already well configured
(package! kubel)
(package! kubel-evil)

(package! plantuml-mode)
(package! json-snatcher)
(package! jsonnet-mode)
(package! emacs-piper
  :recipe (:host gitlab :repo "VincentRavera/emacs-piper" :branch "breadcrumbs"))
(package! ztree)
(package! guix)
(package! shx)
(package! vagrant-tramp)
(package! vagrant)
(package! emacs-calfw  :pin "e3d04c253230ed0692f161f527d4e42686060f62"
  :recipe (:host github :repo "zemaye/emacs-calfw" :branch "master"))

;; GUIX integration
(when (and (getenv "GUIX_PROFILE") (file-directory-p (getenv "GUIX_PROFILE")))
  ;; Vterm
  ;; emacs-libvterm needs to be recompilied by guix to the pinned versions
  (let* (;; locate emacs lisp depo
         (profile-emacs-package (format "%s/share/emacs/site-lisp" (getenv "GUIX_PROFILE")))
         ;; select vterm
         (vterm-path (car
                             (doom-glob (format "%s/vterm*" profile-emacs-package))))
         ;; Build recipe object outside of the package! macro
         (recipe `(:local-repo ,vterm-path)))
    (when vterm-path
      (package! vterm
        :type 'virtual
        :recipe recipe))))
(package! keychain-environment)
(package! ox-jira)
