;; org-roam
(use-package org-roam
  :config
  (setq org-roam-directory (file-truename "~/Documents/org-roam"))
  (org-roam-db-autosync-mode)

  ;; Overwrite default capture template
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" ":PROPERTIES:\n:ID: %(org-id-new)\n:LOGGING: nil\n:END:\n#+title: ${title}\n")
           :unnarrowed t)
          ))
  )

;; org-agenda
(defun org-agenda-refresh ()
  (interactive)
  (setq org-agenda-files (directory-files-recursively "~/Documents/org-roam/" "\\.org$")))
(org-agenda-refresh)

;; TODO keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d!)")
        (sequence "LATER(l)" "NEXT(n)" "WAITING(w)" "ACTIVE(a)" "|" "COMPLETED(c)")))

;; evil org
(use-package evil-org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Prevent TAB issues when using evil-org from terminal.
(setq evil-want-C-i-jump nil)

;; org-bullets
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("▶" "▷" "◉" "○")))

;; org-notifications
(use-package org-notifications
  :config
  (org-notifications-start))

;; Functions for quick access to specific TODO lists.

(defun org-todo-list-LATER ()
  (interactive)
  (org-todo-list "LATER"))

(defun org-todo-list-NEXT ()
  (interactive)
  (org-todo-list "NEXT"))

(defun org-todo-list-TODO ()
  (interactive)
  (org-todo-list "TODO"))

(defun org-todo-list-WAITING ()
  (interactive)
  (org-todo-list "WAITING"))


;; Add custom heights to org-mode section titles
(defun font-height-org-mode-hook ()
  "Stop the org-level headers from increasing in height relative to the other text."
  (set-face-attribute 'org-level-1 nil :height 1.5)
  (set-face-attribute 'org-level-2 nil :height 1.2))
(add-hook 'org-mode-hook #'font-height-org-mode-hook)

;; Disable folding double empty lines
(setq org-cycle-separator-lines -2)

;; Calendar for agenda.
(use-package calfw)
(use-package calfw-org)

;; Unicode characters
(setq cfw:fchar-junction ?╋
      cfw:fchar-vertical-line ?┃
      cfw:fchar-horizontal-line ?━
      cfw:fchar-left-junction ?┣
      cfw:fchar-right-junction ?┫
      cfw:fchar-top-junction ?┯
      cfw:fchar-top-left-corner ?┏
      cfw:fchar-top-right-corner ?┓)

;; Open week view by default.
(defun my--cfw:open-calendar-buffer-view (orig-func &rest args &allow-other-keys)
  (apply orig-func :view 'week :allow-other-keys t args)
  )
(advice-add 'cfw:open-calendar-buffer :around #'my--cfw:open-calendar-buffer-view)

;; Automatic latex previews
(use-package 'org-fragtog
             :config
             (add-hook 'org-mode-hook 'org-fragtog-mode))
