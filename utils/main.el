;;; utils/main.el -*- lexical-binding: t; -*-

(define-suffix-command me/forced-evil:up ()
   :description "Up"
   :transient t
   (interactive)
   (evil-previous-line)
  )

(define-suffix-command me/forced-evil:down ()
   :description "Down"
   :transient t
   (interactive)
   (evil-next-line)
  )

(define-suffix-command me/forced-evil:left ()
   :description "Left"
   :transient t
   (interactive)
   (evil-backward-char)
  )

(define-suffix-command me/forced-evil:right ()
   :description "right"
   :transient t
   (interactive)
   (evil-forward-char)
  )

(define-suffix-command me/forced-evil:scroll-down ()
   :description "Page Down"
   :transient t
   (interactive)
   (scroll-up-command)
  )

(define-suffix-command me/forced-evil:scroll-up ()
   :description "Page Up"
   :transient t
   (interactive)
   (scroll-down-command)
  )

(define-suffix-command me/forced-evil:gg ()
   :description "Begining"
   :transient t
   (interactive)
   (evil-goto-first-line)
  )

(define-suffix-command me/forced-evil:G ()
   :description "End"
   :transient t
   (interactive)
   (evil-goto-line)
  )

(define-transient-command me/forced-evil ()
  "Test Transient Title"
  ["Move"
   ("k" me/forced-evil:up)
   ("j" me/forced-evil:down)
   ("h" me/forced-evil:left)
   ("l" me/forced-evil:right)]
  ["Jump"
   ("C-u" me/forced-evil:scroll-up)
   ("C-d" me/forced-evil:scroll-down)
   ("g" me/forced-evil:gg)
   ("G" me/forced-evil:G)
   ]
  )
