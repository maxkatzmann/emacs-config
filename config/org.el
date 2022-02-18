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

;; Disable state-change logging for todo items.
(setq org-log-done nil)
(setq org-log-repeat nil)

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
