;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Vincent RAVERA"
      user-mail-address "vincent.ravera@atos.net")

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
(setq doom-theme 'doom-sourcerer)

;; NOTE TAKING:
;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/Documents/ORG-ROAM")
;; roam
(setq org-roam-directory "~/Documents/ORG-ROAM")
;; (setq org-roam-graph-viewer "")
(setq deft-directory "~/Documents/ORG-ROAM"
      deft-recursive t)


;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type 'relative)


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
;; (after! 'yasnippet
;;         yas-global-mode 1)

;; ORG
(load! "org-config/org-config.el")

;; Python - Dependency management
;;
;; Pyenv
;;
;; Install pyenv : https://github.com/pyenv/pyenv
;; Then create a recent and global python:
;; pyenv install 3.7.6
;; pyenv global 3.7.6
;;
;; Virtualenvwrapper
;; plugin from: https://github.com/pyenv/pyenv-virtualenv
;; You can now create a venv for your projects
;; (Un-tested, do not use) Install VirtualEnvWrapper: https://virtualenvwrapper.readthedocs.io/en/latest/install.html
;; (Un-tested, do not use) Create a venv for your project:
;; (Un-tested, do not use) mkvirtualenv $NAME_OF_THE_PROJECT
;; (Un-tested, do not use) workon $NAME_OF_THE_PROJECT
;; (Un-tested, do not use) (workon) python -m pip install node pytest pyflakes isort python-language-server ptvsd mypy
;;
;; Starting the IDE
;;
;; Key bindings:

(map! :leader
      (:prefix ("l" . "lsp") ;; LSP UI/DAP
       :desc "lsp start server" "l" #'lsp
       :desc "errors in projects" "e" #'lsp-treemacs-errors-list
       :desc "inline info toggle" "i" #'lsp-ui-sideline-toggle-symbols-info
       :desc "Documentation" "D" #'lsp-ui-doc-glance
       :desc "outline" "o" #'lsp-ui-imenu
       (:prefix ("d" . "debug") ;; DAP-debug
        :desc "start debug" "d" #'dap-debug
        :desc "breakpoint toogle" "d" #'dap-breakpoint-toggle
        :desc "edit debug template" "t" #'dap-debug
        )
       (:prefix ("s" . "search") ;; LSP search
        :desc "definition" "d" #'lsp-ui-peek-find-definitions
        :desc "implementations" "i" #'lsp-ui-peek-find-implementation
        :desc "references" "r" #'lsp-ui-peek-find-references
        )))

;; add current buffer to workspace
(map! :leader
      (:prefix-map ("b" . "buffer")
       (:prefix ("w" . "workspace")
        :desc "Add buffer to workspace" "a" #'persp-add-buffer
        :desc "Delete buffer from workspace" "d" #'persp-remove-buffer))
      (:prefix-map ("TAB" . "workspace")
        :desc "Add buffer to workspace" "a" #'persp-add-buffer
        :desc "Delete buffer from workspace" "D" #'persp-remove-buffer
        :desc "Switch Workspace" "SPC" #'+workspace/switch-to))

(load! "./dap/config.el")
(load! "./lsp/config.el")

;; See https://github.com/murphytalk/doom.d for tips and trick
;;
;; After enabeling the virtualenv (M-x pyenv-workon)
;; Load the lsp (M-x lsp) (SPC l l)
;;

;; Java
;; /home/vravera/Application/Eclipse/lsp/
;; wget http://download.eclipse.org/jdtls/milestones/0.48.0/jdt-language-server-0.48.0-201912040033.tar.gz
;; Usefull tips:
;; lsp (start the daemon)
;; SPC c d - Jump to definition
;; SPC c D - Jump to refrences
;; SPC c a - apply suggestions (Ctrl 1 in eclipse)
;; SPC c j - open resource (Ctrl R in eclipse)
;; lsp-ui-imenu (Show outline of code)
;; lsp-treemacs-error-list (list errors in project)
;;
(load! "./java/config.el")

(load! "./go/config.el")

;; Default browser
(setq browse-url-browser-function 'browse-url-firefox)

;; Default grep command
(setq grep-command "grep --color -nriH --null -e ")

;; loads Eshell config
(load! "./eshell/config.el")

;; loads Kubernetes config
(load! "./kubernetes/config.el")

;; loads PLANTUML config
(load! "./plantuml/config.el")

;; TRAMP config
(after! tramp
  (defun me/tramp/clear ()
    "Shuts down Tramp"
    (interactive)
    (tramp-cleanup-all-connections)
    (tramp-cleanup-all-buffers))
  )

;; undo
(setq global-undo-tree-mode t)
(after! undo-tree
  (setq undo-tree-auto-save-history nil)
  )

;; Piper itegration
(load! "./piper/config.el")
