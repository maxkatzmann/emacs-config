;; counsel
(use-package counsel)

;; ivy
(use-package ivy
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package ivy-rich
  :config
  (ivy-rich-mode 1))

(use-package amx
  :config
  (amx-mode))

;; Company
(use-package company)
