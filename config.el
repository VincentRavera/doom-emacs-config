;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Vincent RAVERA"
      user-mail-address "ravera.vincent@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 14))
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-12"))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
;; 'doom-gruvbox
;; 'doom-sourcerer
;; 'doom-horizon
(setq doom-theme 'doom-feather-dark)

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type 'relative)

;; Disable some nuisances Warnings
(when (or (equal emacs-version "27.1")
          (equal emacs-version "27.2")
          (equal emacs-version "28.1"))
  (setq byte-compile-warnings '(cl-functions)))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; install dependencies
;; sudo add-apt-repository ppa:x4121/ripgrep
;; sudo apt-get install ripgrep
;; SPC s p

;; Transparency
;; (set-frame-parameter (selected-frame) 'alpha '(92 . 90))
;; (add-to-list 'default-frame-alist '(alpha . (92 . 90)))

;; Avy
;; g s SPC for all windows
(setq avy-all-windows t)

;; Spell
;; (after! flyspell
;;   (flyspell-prog-mode))

;; snippets
(after! 'yasnippet
  yas-global-mode 1)

;; Utils Various config
(load! "./utils/config.el")

;; ORG
(load! "org-config/org-config.el")

;; LSP
; DAP Debug configuration
(load! "./dap/config.el")
; LSP specific config
(load! "./lsp/config.el")

;; Java
(load! "./java/config.el")

;; Clojure
(load! "./clojure/config.el")

;; Go
(load! "./go/config.el")

;; Default browser
(setq browse-url-browser-function 'browse-url-firefox)

;; Default grep command
(setq grep-command "grep --color -nriH --null -e ")

;; loads Terminals config
(load! "./term/config.el")

;; loads VTerm config
(load! "./vterm/config.el")

;; loads Eshell config
(load! "./eshell/config.el")

;; loads SHX config
(load! "./shx/config.el")

;; loads Kubernetes config
(load! "./kubernetes/config.el")

;; loads PLANTUML config
(load! "./plantuml/config.el")

;; TRAMP config
(load! "./tramp/config.el")

;; undo
(setq global-undo-tree-mode t)
(after! undo-tree
  (setq undo-tree-auto-save-history nil))

;; Piper integration
(load! "./piper/config.el")

;; Dtache Integration
;; Deprecation warning
;; (load! "./dtache/config.el")

;; Ztree
(after! ztree
  (add-to-list 'ztree-dir-filter-list "\\.pyc$"))

;; Auth
(setq password-cache-expiry (* 60 10))

;; GUIX
(load! "./guix/config.el")

;; magit
(after! magit
  (setq magit-submodule-list-columns (list (list "Path" 25 'magit-modulelist-column-path nil)
                                           (list "Version" 25 'magit-repolist-column-version nil)
                                           (list "Dirt" 4 'magit-repolist-column-dirty
                                                 '((:right-align t)))
                                           (list "CurrentBranch" 20 'magit-repolist-column-branch nil)
                                           (list "#Branchs" 15 'magit-repolist-column-branches
                                                 '((:right-align t)))
                                           (list "#Unpulled" 10 'magit-repolist-column-unpulled-from-upstream
                                                 '((:right-align t)))
                                           (list "#Unpushed" 10 'magit-repolist-column-unpushed-to-upstream
                                                 '((:right-align t)))
                                           ;; (list "B<P" 3 'magit-repolist-column-unpulled-from-pushremote
                                           ;;       '((:right-align t)))
                                           ;; (list "B>P" 3 'magit-repolist-column-unpushed-to-pushremote
                                           ;;       '((:right-align t)))
                                           (list "Stash" 5 'magit-repolist-column-stashes
                                                 '((:right-align t))))))

;; vagrant
(load! "./vagrant/config.el")

;; go-templates
(load! "./go-templates/config.el")

;; EDIFACTs
(load! "./edifact/config.el")
