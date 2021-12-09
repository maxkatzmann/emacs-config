;; ivy

(straight-use-package 'ivy)
(require 'ivy)
(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

(straight-use-package 'counsel)
(straight-use-package 'amx)
(require 'amx)
(amx-mode)
