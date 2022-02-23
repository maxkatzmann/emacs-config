;; org-roam
(straight-use-package 'org-roam)
(require 'org-roam)
(setq org-roam-directory (file-truename "~/Documents/org-roam"))
(org-roam-db-autosync-mode)

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
(straight-use-package 'evil-org)
(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

;; Prevent TAB issues when using evil-org from terminal.
(setq evil-want-C-i-jump nil)

;; org-bullets
(straight-use-package 'org-bullets)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-bullets-bullet-list '("▶" "▷" "◉" "○"))

;; org-notifications
(straight-use-package 'org-notifications)
(org-notifications-start)

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

