(use-package ess)
(use-package ess-view-data)

;; (require 'ess-r-mode)
;; (require 'ess-view-data)

(setq ess-r-backend 'lsp)

(add-hook 'ess-r-mode-hook (lambda ()
                             (lsp-ui-doc-mode -1)))
