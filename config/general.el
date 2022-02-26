;; pasting in visual mode.
(setq-default evil-kill-on-visual-paste nil)

;; Parentheses
(use-package smartparens
  :config
  (smartparens-global-mode t))

;; Unfill
(use-package unfill)

;; Spellcheck
(dolist (hook '(text-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))

;; Default language should be english.
(setq-default ispell-dictionary "english")

;; Undo
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; Projectile
(use-package projectile
  :config
  (projectile-mode +1))

;; Writeroom
(use-package writeroom-mode)

;; Title case
(use-package titlecase)

;; Helpful
(use-package helpful)

;; Exec path from shell -> to make sure that locale is set correctly,
;; which is required by magit.
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-copy-envs '("LANG" "LC_ALL" "LC_CTYPES")))

;; Dired
(when (string= system-type "darwin")
  (setq dired-use-ls-dired t
        insert-directory-program "/usr/local/bin/gls"
        dired-listing-switches "-aBhl --group-directories-first"))

;; Window resizing
(use-package cycle-resize
  :config
  (setq cycle-resize-steps '(75 50 25 50)))
