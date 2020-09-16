;;; ../MY-DOOM/plantuml/config.el -*- lexical-binding: t; -*-

(setq org-plantuml-jar-path (getenv "PLANTUML"))
(after! org
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
  )
