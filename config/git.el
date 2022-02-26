(use-package magit)
(use-package magit-delta)
(use-package magit-todos)

;; Enable delta mode automatically.
(add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))

;; Make it so magit shows changes within lines and not the original block
;; vs. the new block.
(setq magit-diff-refine-hunk (quote all))

;; Git gutter
(use-package git-gutter
  :config
  (add-hook 'prog-mode-hook (lambda () (git-gutter-mode)))
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)
  (set-face-foreground 'git-gutter-fr:modified "dodger blue")
  (set-face-foreground 'git-gutter-fr:added    "sea green")
  (set-face-foreground 'git-gutter-fr:deleted  "red2"))

