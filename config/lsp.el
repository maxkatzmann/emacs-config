(straight-use-package 'flycheck)
(straight-use-package 'company)
(add-hook 'after-init-hook 'global-company-mode)

(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'lsp-ivy)

;; Disable bread crumbs.
(setq lsp-headerline-breadcrumb-enable nil)

;; Enable LSP in TeX mode
(add-hook 'TeX-mode-hook #'lsp)
(add-hook 'ess-r-mode-hook #'lsp)

