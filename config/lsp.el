(straight-use-package 'flycheck)
(straight-use-package 'company)
(add-hook 'after-init-hook 'global-company-mode)

(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'lsp-ivy)

;; Disable bread crumbs.
(setq lsp-headerline-breadcrumb-enable nil)

;; Disable mouse hover
(setq lsp-ui-doc-show-with-mouse nil)

;; Enable LSP in different mode
(add-hook 'TeX-mode-hook #'lsp)
(add-hook 'ess-r-mode-hook #'lsp)
(add-hook 'elm-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)

