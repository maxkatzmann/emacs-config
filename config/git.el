(straight-use-package 'magit)
(straight-use-package 'magit-delta)
(straight-use-package 'magit-todos)
(require 'magit-todos)
(straight-use-package 'diff-hl)
(global-diff-hl-mode)

;; Enable delta mode automatically.
(add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))

;; Make it so magit shows changes within lines and not the original block
;; vs. the new block.
(setq magit-diff-refine-hunk (quote all))
