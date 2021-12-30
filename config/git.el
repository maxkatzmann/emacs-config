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

;; Update diff-hl indicators after before and after magit refreshes.
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
