(straight-use-package 'org-roam)
(require 'org-roam)

(setq org-roam-directory (file-truename "~/Documents/org-roam"))
(org-roam-db-autosync-mode)

(setq org-agenda-files (directory-files-recursively "~/Documents/org-roam" "\\.org$"))
