;;; ../MY-DOOM/lsp/config.el -*- lexical-binding: t; -*-

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
        :desc "breakpoint toogle" "b" #'dap-breakpoint-toggle
        :desc "edit debug template" "t" #'dap-debug-edit-template
        )
       (:prefix ("s" . "search") ;; LSP search
        :desc "definition" "d" #'lsp-ui-peek-find-definitions
        :desc "implementations" "i" #'lsp-ui-peek-find-implementation
        :desc "references" "r" #'lsp-ui-peek-find-references
        )))

(after! (:and lsp python)
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
  ;; (Un-tested, do not use) (workon) python -m pip install node pytest pyflakes isort python-language-server ptvsd mypy flake8
  ;;
  ;; pip install node pytest pyflakes ptvsd mypy flake8 python-lsp-server isort pipenv nose grip
  ;;
  ;; Starting the IDE
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
  )
