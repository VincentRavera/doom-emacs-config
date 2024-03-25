;;; js/config.el -*- lexical-binding: t; -*-


(after! (:and rjsx-mode lsp)
  (setq lsp-typescript-tsserver-plugin-paths
        '("/home/vravera/.nvm/versions/node/v20.11.1/bin/tsc"))
  (setq js-indent-level 4)
  ;; To have eslint to be integrated fully you have no choice but to install it manually
  ;; 1. install nvm https://github.com/nvm-sh/nvm?tab=readme-ov-file#installing-and-updating
  ;; 2. install npm
  ;; nvm install --lts
  ;; 3. install eslint
  ;; npm i -g eslint
  )
