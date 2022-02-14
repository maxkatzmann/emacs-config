(straight-use-package 'python-mode)
(require 'python-mode)

(straight-use-package 'company-jedi)
(straight-use-package 'lsp-pyright)
(straight-use-package 'importmagic)
(straight-use-package 'yapfify)

(add-hook 'python-mode-hook (lambda ()
                              (setq lsp-headerline-breadcrumb-enable nil)
                              (setq lsp-ui-doc-mode -1)
                              (require 'importmagic)
                              (require 'company-jedi)
                              (require 'yapfify)
                              (yapf-mode)))

(setq lsp-enable-file-watchers nil)
