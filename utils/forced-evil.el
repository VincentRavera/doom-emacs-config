;;; utils/main.el -*- lexical-binding: t; -*-

(require 'transient)
;; Examples
;; https://magit.vc/manual/0.1.0/transient.html#Invoking-Transients
;; https://flonic.gitlab.io/org-blog/blog/emacs-transient-tutorial/index.html


(transient-define-prefix me/forced-evil ()
  "Force Evil Keybinding."
  ["Move"
   ("k" "Up"    me/forced-evil:up)
   ("j" "Down"  me/forced-evil:down)
   ("h" "Left"  me/forced-evil:left)
   ("l" "Right" me/forced-evil:right)]
  ["Jump"
   ("C-u" "Up"          me/forced-evil:scroll-up)
   ("C-d" "Down"        me/forced-evil:scroll-down)
   ("g" "BOF"           me/forced-evil:gg)
   ("G" "EOF"           me/forced-evil:G)
   ]
  )

(defun me/forced-evil:up ()
  (interactive)
  (evil-previous-line)
  (me/forced-evil))

(defun me/forced-evil:down ()
  (interactive)
  (evil-next-line)
  (me/forced-evil))

(defun me/forced-evil:left ()
  (interactive)
  (evil-backward-char)
  (me/forced-evil))

(defun me/forced-evil:right ()
  (interactive)
  (evil-forward-char)
  (me/forced-evil))

(defun me/forced-evil:scroll-down ()
  (interactive)
  (scroll-up-command)
  (evil-scroll-up 40)
  (me/forced-evil))

(defun me/forced-evil:scroll-up ()
  (interactive)
  (evil-scroll-down 40)
  (me/forced-evil))

(defun me/forced-evil:gg ()
  (interactive)
  (evil-goto-first-line)
  (me/forced-evil))

(defun me/forced-evil:G ()
  (interactive)
  (evil-goto-line)
  (me/forced-evil))
