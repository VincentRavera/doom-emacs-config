;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
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
(setq doom-font (font-spec :family "monospace" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-one)

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/workspace/wiki/VRA-Notes/")

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
(set-frame-parameter (selected-frame) 'alpha '(92 . 90))
(add-to-list 'default-frame-alist '(alpha . (92 . 90)))

;; Spell
(after! flyspell
  (flyspell-prog-mode))

;; snippets
(after! 'yasnippet
        yas-global-mode 1)

;; ORG
(load! "org-config/org-config.el")

;; Python - Dependency management
;;
;; Prerequisites:
;;
;; Install pyenv : https://github.com/pyenv/pyenv
;; Then create a recent and global python:
;; pyenv install 3.7.6
;; pyenv global 3.7.6
;; Install VirtualEnvWrapper: https://virtualenvwrapper.readthedocs.io/en/latest/install.html
;; Create a venv for your project:
;; mkvirtualenv $NAME_OF_THE_PROJECT
;; workon $NAME_OF_THE_PROJECT
;; (workon) python -m install node pytest pyflakes isort python-language-server ptvsd
;;
;; Starting the IDE
;;
;; After enabeling the virtualenv (M-x pyenv-workon)
;; Load the lsp (M-x lsp)
(load! "dap-configs/dap-configs.el")

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
(after! lsp-java
  (setq lsp-java-server-install-dir "/home/m0b1us/Applications/eclipse/lsp"))
