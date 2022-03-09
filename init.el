;; Straight

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; use-package
(straight-use-package 'use-package)
(use-package straight
             :custom (straight-use-package-by-default t))

;; Nano
(straight-use-package
  '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))
(require 'nano)

;; Load configs.
(add-to-list 'load-path "~/.emacs.d/config/")
(load-library "bazel")
(load-library "completion")
(load-library "elm")
(load-library "git")
(load-library "latex")
(load-library "general")
(load-library "org")
(load-library "protobuf")
(load-library "r")
(load-library "theme")
(load-library "ui")

(load-library "keys")

(load-library "lsp")
(load-library "python-lang")
(load-library "cpp-lang")
