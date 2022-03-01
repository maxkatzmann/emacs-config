(use-package cc-mode)

(use-package clang-format)
(use-package cpp-auto-include)

(add-hook 'c++-mode-hook (lambda ()
                           (setq lsp-ui-doc-mode -1)
                           (require 'clang-format)
                           (require 'cpp-auto-include)))


    
