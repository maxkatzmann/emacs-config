(use-package python-mode)

(use-package company-jedi)
(use-package lsp-pyright)
(use-package importmagic)
(use-package yapfify)

(add-hook 'python-mode-hook (lambda ()
                              (setq lsp-headerline-breadcrumb-enable nil)
                              (setq lsp-ui-doc-mode -1)
                              (require 'importmagic)
                              (require 'company-jedi)
                              (require 'yapfify)
                              (yapf-mode)))

(setq lsp-enable-file-watchers nil)
