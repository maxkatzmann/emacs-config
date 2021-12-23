(straight-use-package 'elm-mode)

;; Format on save
(add-hook 'elm-mode-hook (lambda ()
                           (lsp-ui-doc-mode -1)
                           (elm-format-on-save-mode 1)))
