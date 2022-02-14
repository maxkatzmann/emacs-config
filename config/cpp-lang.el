(straight-use-package 'cc-mode)
(require 'cc-mode)

(straight-use-package 'clang-format)
(straight-use-package 'cpp-auto-include)

(add-hook 'c++-mode-hook (lambda ()
                           (setq lsp-ui-doc-mode -1)
                           (require 'clang-format)
                           (require 'cpp-auto-include)))


    
