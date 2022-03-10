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

(straight-use-package 'use-package)
(use-package straight
             :custom (straight-use-package-by-default t))

(straight-use-package 'org)

(setq-default evil-kill-on-visual-paste nil)

(use-package unfill)

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package titlecase)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-copy-envs '("LANG" "LC_ALL" "LC_CTYPES")))

(use-package writeroom-mode)

(use-package helpful)

(use-package cycle-resize
  :config
  (setq cycle-resize-steps '(75 50 25 50)))

(straight-use-package
  '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))
(require 'nano)
(require 'nano-theme-light)
(require 'nano-theme-dark)

(setq nano-font-family-monospaced "Roboto Mono")

(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size 256000)

(setq default-frame-alist
      (append (list
	           '(min-height . 1)
               '(height     . 45)
	           '(min-width  . 1)
               '(width      . 81)
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 24)
               '(left-fringe    . 8)
               '(right-fringe   . 8)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0))))

(defun nano-refresh-theme ()
   "Convenience function which refreshes the nano-theme.
 Calls \(nano-faces\) and \(nano-theme\) sequentially."
   (interactive)
   (progn
     (nano-faces)
     (nano-theme)))

 (defcustom nano-theme-var "light"
   "Variable which sets the default startup theme as light or dark.
 Also allows for toggling of the themes. Is set to 'light' by
 'nano-theme-light' and 'dark' by 'nano-theme-dark'.
 Defaults to nil."
   :group 'nano
   :type 'string)

 (defun nano-toggle-theme ()
   "Function to interactively toggle between light and dark nano themes.
 Requires both to be loaded in order to work."
   (interactive)
   (cond ((string= nano-theme-var "light")
          (progn (nano-theme-set-dark)
                 (nano-refresh-theme)
                 (setq nano-theme-var "dark")
                 ;; Make sure org font sizes are updated after refreshing the
                 ;; theme.
                 (font-height-org-mode-hook)))
          ((string= nano-theme-var "dark")
          (progn (nano-theme-set-light)
                 (nano-refresh-theme)
                 (setq nano-theme-var "light")
                 ;; Make sure org font sizes are updated after refreshing the
                 ;; theme.
                 (font-height-org-mode-hook)))
          (t nil)))

(nano-theme-set-light)
(nano-refresh-theme)

(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
(add-hook 'term-mode-hook (lambda () (display-line-numbers-mode 0)))

(setq-default line-spacing 2)
(setq default-text-properties '(line-spacing 0.2 line-height 1.2))

(global-hl-line-mode 1)

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook (lambda () (rainbow-delimiters-mode))))

(defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
(defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
(defconst day-re "[A-Za-z]\\{2,3\\}")

(defun svg-progress-percent (value)
  (svg-image (svg-lib-concat
              (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
              (svg-lib-tag (concat value "%")
                           nil :stroke 0 :margin 0)) :ascent 'center))

(defun svg-progress-count (value)
  (let* ((seq (mapcar #'string-to-number (split-string value "/")))
         (count (float (car seq)))
         (total (float (cadr seq))))
  (svg-image (svg-lib-concat
              (svg-lib-progress-bar (/ count total) nil
                                    :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
              (svg-lib-tag value nil
                           :stroke 0 :margin 0)) :ascent 'center)))

(use-package counsel)

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

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package smartparens
  :config
  (smartparens-global-mode t))

(dolist (hook '(text-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))

(setq-default ispell-dictionary "english")

(defun insert-current-date () (interactive)
    (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(use-package magit)
(use-package magit-delta)
(use-package magit-todos)

(add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))

(use-package git-gutter
  :config
  (add-hook 'prog-mode-hook (lambda () (git-gutter-mode)))
  (add-hook 'TeX-mode-hook (lambda () (git-gutter-mode)))
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)
  (set-face-foreground 'git-gutter-fr:modified "dodger blue")
  (set-face-foreground 'git-gutter-fr:added    "sea green")
  (set-face-foreground 'git-gutter-fr:deleted  "red2"))

(use-package projectile
  :config
  (projectile-mode +1))

(when (string= system-type "darwin")
  (setq dired-use-ls-dired t
        insert-directory-program "/usr/local/bin/gls"
        dired-listing-switches "-aBhl --group-directories-first"))

(use-package bazel
  :config
  (setq bazel-buildifier-before-save t))

(use-package cc-mode)
(use-package clang-format)
(use-package cpp-auto-include)

(add-hook 'c++-mode-hook (lambda ()
                           (setq lsp-ui-doc-mode -1)
                           (require 'clang-format)
                           (require 'cpp-auto-include)))

(use-package elm-mode)

(add-hook 'elm-mode-hook (lambda ()
                           (lsp-ui-doc-mode -1)
                           (elm-format-on-save-mode 1)))

(straight-use-package 'auctex)
(use-package ivy-bibtex)

(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin/"))
(setq exec-path (append exec-path '("/Library/TeX/texbin/")))

(setq TeX-error-overview-open-after-TeX-run t)

(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-start-server t)
(setq TeX-source-correlate-method 'synctex)

(setq TeX-view-program-list
      '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
(setq TeX-view-program-selection '((output-pdf "Skim")))

(add-hook 'TeX-mode-hook (lambda ()
                           (lsp-ui-doc-mode -1)
                           (setq fill-column 70)))

(setq sentence-end-double-space t)

(use-package org-ref)

(require 'cl-lib)
(setq bibtex-autokey-before-presentation-function
  (lambda
    (key)
    (concat
     (seq-subseq key 0
                 (cl-search "-" key))
     "-"
     (seq-subseq key
             (+
              ;; TODO: We need to handle the case where we get nil here.
              (cl-search "-" key)
              4))
     "-"
     (seq-subseq key
             (+
              ;; TODO: We need to handle the case where we get nil here.
              (cl-search "-" key)
              1)
             (+
              ;; TODO: We need to handle the case where we get nil here.
              (cl-search "-" key)
              3)))))

(setq bibtex-autokey-name-length -1)
(setq bibtex-autokey-name-year-separator "-")
(setq bibtex-autokey-names 3)
(setq bibtex-autokey-names-stretch 1)
(setq bibtex-autokey-titleword-length -1)
(setq bibtex-autokey-titleword-separator "")
(setq bibtex-autokey-year-title-separator "-")

(add-hook 'bibtex-mode-hook (lambda ()
                              (display-line-numbers-mode)
                              (setq display-line-numbers 'relative)))

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(setq reftex-plug-into-AUCTeX t)

(setq reftex-external-file-finders
      '(("tex" . "kpsewhich -format=.tex %f")
        ("bib" . "kpsewhich -format=.bib %f")))

(setq reftex-format-cite-function 
  '(lambda (key fmt)
     (let ((cite (replace-regexp-in-string "%l" key fmt)))
       (if (or (= ?~ (string-to-char fmt))
               (member (preceding-char) '(?\ ?\t ?\n ?~ ?{ ?,))
               (member (following-char) '(?} ))
     )
           cite
         (concat "~" cite)))))

(use-package auctex-latexmk
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  (auctex-latexmk-setup)
  (setq TeX-command-default "LatexMk")
  (setq latex-build-command "LatexMk"))

(defun latex/build ()
  (interactive)
  (progn
    (let ((TeX-save-query nil))
      (TeX-save-document (TeX-master-file)))
    (TeX-command latex-build-command 'TeX-master-file -1)))

(defun latex/font-bold () (interactive) (TeX-font nil ?\C-b))
(defun latex/font-medium () (interactive) (TeX-font nil ?\C-m))
(defun latex/font-code () (interactive) (TeX-font nil ?\C-t))
(defun latex/font-emphasis () (interactive) (TeX-font nil ?\C-e))
(defun latex/font-italic () (interactive) (TeX-font nil ?\C-i))
(defun latex/font-clear () (interactive) (TeX-font nil ?\C-d))
(defun latex/font-calligraphic () (interactive) (TeX-font nil ?\C-a))
(defun latex/font-small-caps () (interactive) (TeX-font nil ?\C-c))
(defun latex/font-sans-serif () (interactive) (TeX-font nil ?\C-f))
(defun latex/font-normal () (interactive) (TeX-font nil ?\C-n))
(defun latex/font-serif () (interactive) (TeX-font nil ?\C-r))
(defun latex/font-oblique () (interactive) (TeX-font nil ?\C-s))
(defun latex/font-upright () (interactive) (TeX-font nil ?\C-u))

(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'lsp-ivy)

(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-ui-doc-show-with-mouse nil)

(add-hook 'TeX-mode-hook #'lsp)
(add-hook 'ess-r-mode-hook #'lsp)
(add-hook 'elm-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package markdown-preview-mode)

(use-package protobuf-mode)

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

(use-package ess)
(use-package ess-view-data)

(setq ess-r-backend 'lsp)

(add-hook 'ess-r-mode-hook (lambda ()
                             (lsp-ui-doc-mode -1)))

(defun mk/org-mode-setup ()
  (org-indent-mode))

(use-package org
  :hook (org-mode . mk/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"))

(defun font-height-org-mode-hook ()
  "Stop the org-level headers from increasing in height relative to the other text."
  (set-face-attribute 'org-level-1 nil :height 1.5)
  (set-face-attribute 'org-level-2 nil :height 1.2))
(add-hook 'org-mode-hook #'font-height-org-mode-hook)

(setq org-cycle-separator-lines -2)

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("bib" . "src bibtex"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))

(defun org-todo-list-LATER ()
  (interactive)
  (org-todo-list "LATER"))

(defun org-todo-list-NEXT ()
  (interactive)
  (org-todo-list "NEXT"))

(defun org-todo-list-TODO ()
  (interactive)
  (org-todo-list "TODO"))

(defun org-todo-list-WAITING ()
  (interactive)
  (org-todo-list "WAITING"))

(defun org-agenda-BOARD ()
  (interactive)
  (org-agenda nil "w"))

(use-package org-roam
  :config
  (setq org-roam-directory (file-truename "~/Documents/org-roam"))
  (org-roam-db-autosync-mode)

  ;; Overwrite default capture template
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "${slug}.org" ":PROPERTIES:\n:ID: %(org-id-new)\n:LOGGING: nil\n:END:\n#+STARTUP: latexpreview\n#+title: ${title}\n")
           :unnarrowed t)
          ))
  )

(defun org-agenda-refresh ()
  (interactive)
  (setq org-agenda-files (directory-files-recursively "~/Documents/org-roam/" "\\.org$")))
(org-agenda-refresh)

(setq org-deadline-warning-days 14)

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d!)")
        (sequence "LATER(l)" "NEXT(n)" "WAITING(w)" "ACTIVE(a)" "|" "COMPLETED(c)")))

(setq org-agenda-start-on-weekday 1)
(setq calendar-week-start-day 1)

(setq org-agenda-custom-commands
 '(("w" "Workflow Status"
    ((todo "NEXT"
          ((org-agenda-overriding-header "Things to do next")
           (org-agenda-todo-list-sublevels nil)
           (org-agenda-files org-agenda-files)))
     (todo "TODO"
          ((org-agenda-overriding-header "Not pressing")
           (org-agenda-files org-agenda-files)))
     (todo "WAITING"
          ((org-agenda-overriding-header "Waiting for External")
           (org-agenda-files org-agenda-files)))
     (todo "LATER"
          ((org-agenda-overriding-header "Backlog")
           (org-agenda-files org-agenda-files))) 
    ))))

(use-package evil-org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(setq evil-want-C-i-jump nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(defun mk/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/config.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'mk/org-babel-tangle-config)))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("▶" "▷" "◉" "○")))

(use-package calfw)
(use-package calfw-org)

(setq cfw:fchar-junction ?╋
      cfw:fchar-vertical-line ?┃
      cfw:fchar-horizontal-line ?━
      cfw:fchar-left-junction ?┣
      cfw:fchar-right-junction ?┫
      cfw:fchar-top-junction ?┯
      cfw:fchar-top-left-corner ?┏
      cfw:fchar-top-right-corner ?┓)

(defun my--cfw:open-calendar-buffer-view (orig-func &rest args &allow-other-keys)
  (apply orig-func :view 'week :allow-other-keys t args)
  )
(advice-add 'cfw:open-calendar-buffer :around #'my--cfw:open-calendar-buffer-view)

(use-package org-fragtog
  :config
  (add-hook 'org-mode-hook 'org-fragtog-mode))

(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

(use-package org-notifications
  :config
  (org-notifications-start))

(setq mac-option-modifier 'alt)
(global-set-key (kbd "A-<backspace>") 'backward-kill-word)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package evil
  :init
  (setq evil-want-keybinding nil) ;; Required for evil-collection
  (setq evil-want-visual-char-semi-exclusive t)
  :config
  (evil-mode 1)
  ;; Tell evil to use undo-tree
  (evil-set-undo-system 'undo-tree))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
  (evil-define-key 'visual global-map "s" 'evil-surround-region))

(use-package evil-exchange
  :config
  (evil-exchange-install))

(use-package which-key
  :config
  (which-key-setup-minibuffer)
  (which-key-mode))

(use-package iedit
  :config
  (straight-use-package 'evil-iedit-state)
  (require 'evil-iedit-state))

(define-key evil-normal-state-map "/" 'swiper)

(use-package hydra)

(use-package dash)
(use-package general)
(use-package bind-map)
(use-package bind-key)
(straight-use-package
 '(spaceleader :type git :host github :repo "mohkale/spaceleader"))

(leader-set-keys
  "TAB" '(switch-to-last-buffer+ :wk "last-buffer")
  "SPC" '(counsel-M-x :wk "M-x")
  "<escape>" 'abort-recursive-edit
  "DEL"      'exit-recursive-edit
)

(leader-set-keys
  "a" '(:ignore t :wk "applications")
  "ad" 'dired
)

(leader-set-keys-for-major-mode 'bazel-mode "=" 'bazel-buildifier)

(leader-set-keys-for-major-mode 'bibtex-mode "s" 'org-ref-sort-bibtex-entry)
(leader-set-keys-for-major-mode 'bibtex-mode "c" 'bibtex-clean-entry)

(defun create-scratch-buffer nil
   "create a scratch buffer"
   (interactive)
   (switch-to-buffer (get-buffer-create "*scratch*")))
   ;; (lisp-interaction-mode))        

(leader-set-keys
  "b" '(:ignore t :wk "buffers")
  "bb" 'switch-to-buffer
  "bd" 'kill-this-buffer
  "bm" 'buffer-menu
  "bn" 'next-buffer
  "bp" 'previous-buffer
  "bs" 'create-scratch-buffer
  "br" 'revert-buffer
)

(defun comment-beginning-of-line ()
  (interactive)
  (comment-line 1)
  (previous-line))

(leader-set-keys
  "c" '(:ignore t :wk "comment")
  "cl" 'comment-beginning-of-line
)

(leader-set-keys-for-major-mode 'c++-mode "gd" 'lsp-find-definition)
(leader-set-keys-for-major-mode 'c++-mode "=" 'lsp-format-buffer)

(leader-set-keys
  "e" '(:ignore t :wk "eval")
  "es" 'eval-last-sexp
)

(defun mk/find-user-init-file ()
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/config.org")))

(leader-set-keys
  "f" '(:ignore t :wk "files")
  "ff" 'counsel-find-file
  "fc" 'copy-file
  "fh" 'find-file-at-point
  "fed" 'mk/find-user-init-file
)

(leader-set-keys
  "g" '(:ignore t :wk "git")
  "gs" 'magit-status
  "gh" 'magit-diff-buffer-file
  "gm" '(:ignore t :wk "merge")
  "gmn" 'smerge-next
  "gmp" 'smerge-prev
  "gma" 'smerge-keep-all
  "gmc" 'smerge-keep-current
  "gmo" 'smerge-keep-other
)

(leader-set-keys
  "h" '(:ignore t :wk "hel")
  "hv" 'helpful-variable
  "hf" 'helpful-function
  "ht" 'helpful-at-point
)

(add-hook 'lsp-mode-hook (lambda ()
                           (leader-set-keys
                             "mj" 'lsp-ivy-workspace-symbol)))

(leader-set-keys
  "o" '(:ignore t :wk "org-roam")
  "oa" '(:ignore t :wk "agenda")
  "oat" 'org-todo-list
  "oaT" 'org-todo-list-TODO
  "oaN" 'org-todo-list-NEXT
  "oaL" 'org-todo-list-LATER
  "oaW" 'org-todo-list-WAITING
  "oaB" 'org-agenda-BOARD
  "oal" 'org-agenda-list
  "oac" 'cfw:open-org-calendar
  "oar" 'org-agenda-refresh
  "ob" 'org-roam-buffer-toggle
  "of" 'org-roam-node-find
)

(defun org-fold-all-task-entries ()
  "Close/fold all entries marked that represent tasks."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (while (outline-previous-heading)
      (when (or (org-entry-is-todo-p) (org-entry-is-done-p))
        (hide-entry)))))

(leader-set-keys-for-major-mode 'org-mode "t" 'org-todo)
(leader-set-keys-for-major-mode 'org-mode "f" 'org-fold-all-task-entries)
(leader-set-keys-for-major-mode 'org-mode "s" 'org-schedule)
(leader-set-keys-for-major-mode 'org-mode "d" 'org-deadline)
(leader-set-keys-for-major-mode 'org-mode "L" 'org-shiftright)
(leader-set-keys-for-major-mode 'org-mode "H" 'org-shiftleft)
(leader-set-keys-for-major-mode 'org-mode "K" 'org-shiftup)
(leader-set-keys-for-major-mode 'org-mode "J" 'org-shiftdown)
(leader-set-keys-for-major-mode 'org-mode "S" 'org-sort-entries)
(leader-set-keys-for-major-mode 'org-mode "it" 'org-insert-todo-heading)
(leader-set-keys-for-major-mode 'org-mode "in" 'org-roam-node-insert)
(leader-set-keys-for-major-mode 'org-mode "il" 'org-insert-link)
(leader-set-keys-for-major-mode 'org-mode "ic" 'mk/org-insert-src-block)
(leader-set-keys-for-major-mode 'org-mode "o" 'org-open-at-point)
(leader-set-keys-for-major-mode 'org-mode "j" 'counsel-imenu)

(leader-set-keys
  "/" 'projectile-grep
)

(leader-set-keys-for-major-mode 'python-mode "=" 'yapfify-buffer)

(leader-set-keys-for-major-mode 'ess-r-mode "s" 'R)
(leader-set-keys-for-major-mode 'ess-r-mode "c" 'ess-eval-buffer)
(leader-set-keys-for-major-mode 'ess-r-mode "=" 'lsp-format-buffer)

(leader-set-keys-for-major-mode 'shell-mode "h" 'counsel-shell-history)

(leader-set-keys
  "S" '(:ignore t :wk "Spelling")
  "Sc" 'flyspell-auto-correct-word
)

(leader-set-keys
  "s" '(:ignore t :wk "subsitute")
  "se" '(evil-iedit-state/iedit-mode)
  "sr" 'sp-rewrap-sexp
  "sd" 'sp-splice-sexp
)

(leader-set-keys-for-major-mode 'latex-mode "c" 'latex/build)
(leader-set-keys-for-major-mode 'latex-mode "b" 'TeX-command-master)
(leader-set-keys-for-major-mode 'latex-mode "v" 'TeX-view)
(leader-set-keys-for-major-mode 'latex-mode "m" 'TeX-insert-macro)
(leader-set-keys-for-major-mode 'latex-mode "e" 'LaTeX-environment)
(leader-set-keys-for-major-mode 'latex-mode "l" 'TeX-error-overview)
(leader-set-keys-for-major-mode 'latex-mode "-" 'TeX-recenter-output-buffer)
(leader-set-keys-for-major-mode 'latex-mode "r" 'reftex-reference)
(leader-set-keys-for-major-mode 'latex-mode "s" 'LaTeX-section)
(leader-set-keys-for-major-mode 'latex-mode "C" 'reftex-citation)
(leader-set-keys-for-major-mode 'latex-mode "xb" 'latex/font-bold)
(leader-set-keys-for-major-mode 'latex-mode "xe" 'latex/font-emphasis)
(leader-set-keys-for-major-mode 'latex-mode "xi" 'latex/font-italic)
(leader-set-keys-for-major-mode 'latex-mode "xc" 'latex/font-code)
(leader-set-keys-for-major-mode 'latex-mode "xs" 'latex/font-small-caps)

(leader-set-keys
  "x" '(:ignore t :wk "text")
  "xp" 'fill-paragraph
  "xP" 'unfill-paragraph
  "xC" 'capitalize-word
  "xL" 'downcase-word
  "xT" 'titlecase-region
)

(leader-set-keys
  "T" '(:ignore t :wk "Theme")
  "Ts" 'nano-toggle-theme
)

(leader-set-keys
  "t" '(:ignore t :wk "toggles")
  "ta" 'auto-fill-mode
  "tl" 'toggle-truncate-lines
)

(leader-set-keys
  "u" 'universal-argument
)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun split-to-shell ()
  (interactive)
  (split-and-follow-horizontally)
  (evil-window-move-very-bottom)
  (unless (get-buffer "*shell*")
    (shell))
  (switch-to-buffer "*shell*"))

;; Transient state for window resizing
(defhydra hydra-transient-window-resize (:timeout 4)
  "resize window vertically"
  ("s" cycle-resize-window-vertically "resize vertically")
  ("v" cycle-resize-window-horizontally "resize horizontally"))

(leader-set-keys
  "w" '(:ignore t :wk "window")
  "wd" 'delete-window
  "wv" 'split-and-follow-vertically
  "ws" 'split-and-follow-horizontally
  "wl" 'evil-window-right
  "wL" 'evil-window-move-far-right
  "wh" 'evil-window-left
  "wH" 'evil-window-move-far-left
  "wj" 'evil-window-down
  "wJ" 'evil-window-move-very-bottom
  "wk" 'evil-window-up
  "wK" 'evil-window-move-very-top
  "wt" 'split-to-shell
  "wr" 'hydra-transient-window-resize/body
)

(leader-set-keys
  "z" '(:ignore t :wk "zoom")
  "zx" 'text-scale-adjust
)
