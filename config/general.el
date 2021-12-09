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

