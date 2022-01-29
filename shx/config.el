;;; shx/config.el -*- lexical-binding: t; -*-

(shx-global-mode 1)

(map! :map doom-leader-open-map
      "s" #'+shell/toggle
      "S" #'+shell/here)

