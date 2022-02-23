;; pasting in visual mode.
(setq-default evil-kill-on-visual-paste nil)

;; Parentheses
(straight-use-package 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode t)

;; Unfill
(straight-use-package 'unfill)

;; Spellcheck
(dolist (hook '(text-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))

;; Default language should be english.
(setq-default ispell-dictionary "english")

;; Undo
(straight-use-package 'undo-tree)
(global-undo-tree-mode)

;; Projectile
(straight-use-package 'projectile)
(projectile-mode +1)

;; Writeroom
(straight-use-package 'writeroom-mode)
(require 'writeroom-mode)

;; Title case
(straight-use-package 'titlecase)
(require 'titlecase)

;; Helpful
(straight-use-package 'helpful)
(require 'helpful)

;; Exec path from shell -> to make sure that locale is set correctly,
;; which is required by magit.
(straight-use-package 'exec-path-from-shell)
(require 'exec-path-from-shell)
(exec-path-from-shell-copy-envs '("LANG" "LC_ALL" "LC_CTYPES"))

;; Dired
(when (string= system-type "darwin")
  (setq dired-use-ls-dired t
        insert-directory-program "/usr/local/bin/gls"
        dired-listing-switches "-aBhl --group-directories-first"))
