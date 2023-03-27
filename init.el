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

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(straight-use-package 'org)

(server-start)

(setq evil-want-keybinding nil)

(load "~/.emacs.d/themes/github-dark-theme.el")
(load "~/.emacs.d/themes/github-light-theme.el")

(use-package auto-dark
  :custom
  (auto-dark-dark-theme 'github-dark)
  (auto-dark-light-theme 'github-light))

(defun mk/minibuffer-dark-bg ()
  (set (make-local-variable 'face-remapping-alist)
       '((default :background "#444d56"))))

(defun mk/minibuffer-light-bg ()
  (set (make-local-variable 'face-remapping-alist)
       '((default :background "#d7d7d7"))))

(defun mk/dark-theme ()
  (interactive)
  (remove-hook 'minibuffer-setup-hook 'mk/minibuffer-light-bg)
  (add-hook 'minibuffer-setup-hook 'mk/minibuffer-dark-bg))

(defun mk/light-theme ()
  (interactive)
  (remove-hook 'minibuffer-setup-hook 'mk/minibuffer-dark-bg)
  (add-hook 'minibuffer-setup-hook 'mk/minibuffer-light-bg))

(add-hook 'auto-dark-dark-mode-hook 'mk/dark-theme)
(add-hook 'auto-dark-light-mode-hook 'mk/light-theme)

(add-hook 'after-make-frame-functions
  (lambda (frame)
    (select-frame frame)
    (when (display-graphic-p frame)
      (auto-dark-mode t))))

(straight-use-package
    '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))
(require 'nano-layout)

(straight-use-package
    '(nano-modeline :type git :host github :repo "rougier/nano-modeline"))
(nano-modeline-mode)

(set-face-attribute 'default nil
  :family "Noto Mono"
  :height 120
  :weight 'medium
  :width 'normal)

(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
(add-hook 'term-mode-hook (lambda () (display-line-numbers-mode 0)))

(setq-default line-spacing 2)
(setq default-text-properties '(line-spacing 0.2 line-height 1.2))

(global-hl-line-mode 1)

(use-package consult)

(use-package consult-eglot)

(straight-use-package
    '(consult-reftex :type git :host github :repo "karthink/consult-reftex"))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package amx
  :config
  (amx-mode))

(use-package company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 3)
(setq company-dabbrev-downcase 'case-replace)

(defun mk/company-clear-on-empty-prefix (candidates)
  (if (= (length company-prefix) 0)
    nil
    candidates))
(setq company-transformers '(mk/company-clear-on-empty-prefix))

(defun mk/company-backends-hook ()
  (interactive)
  (message "mk/ Hooking backends...")
  (setq company-backends
  '((company-capf
     company-files
     company-keywords
     company-dabbrev-code
     company-dabbrev
     company-clang
     company-gtags
     company-etags
     company-semantic
     company-bbdb
     :separate))))
(add-hook 'text-mode-hook 'mk/company-backends-hook)
(add-hook 'prog-mode-hook 'mk/company-backends-hook)
(add-hook 'eglot-managed-mode-hook (lambda () (mk/company-backends-hook)))

(use-package smartparens
  :config
  (sp-pair "$" "$")
  (smartparens-global-mode t))

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(use-package flyspell-correct
  :after flyspell)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(setq ispell-dictionary "english")

(use-package consult-flyspell
  :straight (consult-flyspell :type git :host gitlab :repo "OlMon/consult-flyspell" :branch "master")
  :config
  ;; default settings
  (setq consult-flyspell-correct-function '(lambda () (flyspell-correct-at-point) (consult-flyspell))
        consult-flyspell-set-point-after-word t
        consult-flyspell-always-check-buffer nil))

(use-package dirvish
  :config
  (dirvish-override-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
  "H" 'dired-hide-dotfiles-mode
  "h" 'dired-up-directory
  "l" 'dired-find-file
  (kbd "SPC") 'counsel-M-x))

(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))

(use-package magit)

(use-package magit-delta)
(add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))

(use-package hl-todo
    :init (setq global-hl-todo-mode t)
          (setq hl-todo-keyword-faces
            '(("TODO"   . "#FF0000")
              ("\\todo"   . "#FF0000")
              ("FIXME"  . "#FF0000")
              ("DEBUG"  . "#A020F0")
              ("GOTCHA" . "#FF4500")
              ("STUB"   . "#1E90FF"))))

(use-package magit-todos
  :init (magit-todos-mode))



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

(use-package citar
  :bind (("C-c b" . citar-insert-citation)
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset)))

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

(use-package org-ref)

(use-package clang-format
  :init (setq clang-format-style "google"))

(defun mk/clang-format-save-hook-for-this-buffer ()
  "Create a buffer local save hook."
  (add-hook 'before-save-hook
	    (lambda ()
	      (clang-format-buffer)
	      ;; Continue to save.
	      nil)
	    nil
	    ;; Buffer local hook.
	    t))

(add-hook 'c-mode-hook (lambda () (mk/clang-format-save-hook-for-this-buffer)))
(add-hook 'c++-mode-hook (lambda () (mk/clang-format-save-hook-for-this-buffer)))

(use-package csv-mode)

(use-package tex
  :straight auctex
  :mode(("lua_.*" . LaTeX-mode)))

(straight-use-package
  '(consult-reftex :type git :host github :repo "karthink/consult-reftex"))

(add-hook 'TeX-mode-hook 'eglot-ensure)

(use-package auctex-latexmk
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  (auctex-latexmk-setup)
  (setq TeX-command-default "LatexMk")
  (setq latex-build-command "LatexMk")
  (setq LaTeX-electric-left-right-brace t)) ; Enable left/right auto-complete

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

(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-start-server t)
(setq TeX-source-correlate-method 'synctex)

(setq TeX-view-program-list
  '(("Zathura"
                 ("zathura "
                  (mode-io-correlate " --synctex-forward %n:0:%b -x \"emacsclient +%{line} %{input}\" ")
                  " %o")
                 "zathura")))
(setq TeX-view-program-selection '((output-pdf "Zathura")))

(add-hook 'TeX-mode-hook (lambda ()
                           (setq fill-column 70)))

(defun mk/align-latex-table ()
  (interactive)
  (unless (string= (LaTeX-current-environment) "document")
    (let ((s (make-marker))
          (e (make-marker)))
      (set-marker s (save-excursion
                      (LaTeX-find-matching-begin)
                      (forward-line)
                      (point)))
      (set-marker e (save-excursion
                      (LaTeX-find-matching-end)
                      (forward-line -1)
                      (end-of-line)
                      (point)))
      ;; Delete the next 2 lines if you don't like indenting and removal
      ;; of whitespaces:
      (LaTeX-fill-environment nil)
      (whitespace-cleanup-region s e)
      (align-regexp s e "\\(\\s-*\\)&" 1 1 t)
      (align-regexp s e "\\(\\s-*\\)\\\\\\\\")
      (set-marker s nil)
      (set-marker e nil))))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package org)

(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("bib" . "src bibtex"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("tex" . "src latex"))
(add-to-list 'org-structure-template-alist '("ein" . "src ein-python :session localhost"))
(add-to-list 'org-structure-template-alist '("r" . "src R :session :exports both results output org"))

(org-babel-do-load-languages
  'org-babel-load-languages
    '((emacs-lisp . t)
      (python . t)
      (R . t)))

(setq org-confirm-babel-evaluate nil)

(defun mk/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/config.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'mk/org-babel-tangle-config)))

(use-package python-mode
  :init
  (add-hook 'python-mode-hook 'eglot-ensure)) ;; Enable LSP in Python

(use-package yapfify
  :hook (python-mode . yapf-mode)
  :config (setq yapfify-executable "~/.emacs.d/yapfify.sh"))

(use-package ess)

(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(use-package eglot)

(setq eglot-server-programs
  '((python-mode . ("pylsp"))
    (latex-mode . ("texlab"))))

(use-package current-window-only
  :straight (current-window-only
             :type git
             :host github
             :repo "FrostyX/current-window-only")
  :config
  (current-window-only-mode))

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
  
(defun mk/split-to-shell ()
  "If no *shell* buffer exists, one is created using the `shell` function
  and it is displayed in a new window at the ver bottom. If such a window is
  shown already, it is deleted instead."
  (interactive)
  (let ((shell-window (get-buffer-window "*shell*")))
    (if shell-window
       (progn
         (delete-window shell-window))
        (progn
        (split-and-follow-horizontally)
        (evil-window-move-very-bottom)
        (unless (get-buffer "*shell*")
          (shell))
        (switch-to-buffer "*shell*")))))

(defun mk/split-to-shell-fullscreen ()
  (interactive)
  (mk/split-to-shell)
  (delete-other-windows))

(use-package rainbow-mode)

(use-package helpful)

(setq-default evil-kill-on-visual-paste nil)

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq-default auto-fill-function 'do-auto-fill)

(use-package unfill)

(defun mk/replace-char-under-cursor-with-char (new-char)
  (insert new-char)
  (delete-char 1))

(defun mk/convert-to-umlaut ()
  (interactive)
  (let ((to-replace (string (char-after))))
    (cond ((string= to-replace "a")
	   (mk/replace-char-under-cursor-with-char "ä"))
	  ((string= to-replace "A")
	   (mk/replace-char-under-cursor-with-char "Ä"))
	  ((string= to-replace "o")
	   (mk/replace-char-under-cursor-with-char "ö"))
	  ((string= to-replace "O")
	   (mk/replace-char-under-cursor-with-char "Ö"))
	  ((string= to-replace "u")
	   (mk/replace-char-under-cursor-with-char "ü"))
	  ((string= to-replace "U")
	   (mk/replace-char-under-cursor-with-char "Ü"))
	  ((string= to-replace "s")
	   (mk/replace-char-under-cursor-with-char "ß")))))

(defun mk/space-end-2-1 ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\.  \\([^ ]\\)" nil t)
      (replace-match ". \\1" t))))

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

(use-package rg)

(use-package evil-iedit-state)

(setq mac-option-modifier 'alt)
(setq mac-command-modifier 'meta)
(global-set-key (kbd "A-<backspace>") 'backward-kill-word)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package evil
  :init
  (setq evil-want-keybinding nil) ;; Required for evil-collection
  (setq evil-want-visual-char-semi-exclusive t)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree))

(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-exchange
  :config
  (evil-exchange-install))

(use-package evil-matchit
  :init
  (global-evil-matchit-mode 1))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
  (evil-define-key 'visual global-map "s" 'evil-surround-region))

(use-package which-key
  :config
  (which-key-setup-minibuffer)
  (which-key-mode))

(use-package hydra)

(use-package dash)
(use-package general)
(use-package bind-map)
(use-package bind-key)
(straight-use-package
 '(spaceleader :type git :host github :repo "mohkale/spaceleader"))

(leader-set-keys
  "TAB" '(switch-to-last-buffer+ :wk "last-buffer")
  "SPC" 'execute-extended-command
  "<escape>" 'abort-recursive-edit
  "DEL" 'exit-recursive-edit
  "/" 'consult-ripgrep
)

(define-key evil-normal-state-map "/" 'consult-line)

(leader-set-keys-for-major-mode 'bibtex-mode "s" 'org-ref-sort-bibtex-entry)
(leader-set-keys-for-major-mode 'bibtex-mode "c" 'bibtex-clean-entry)

(defun create-scratch-buffer nil
   "create a scratch buffer"
   (interactive)
   (switch-to-buffer (get-buffer-create "*scratch*")))

(leader-set-keys
  "b" '(:ignore t :wk "buffers")
  "bb" 'consult-buffer
  "bd" 'kill-this-buffer
  "bn" 'next-buffer
  "bp" 'previous-buffer
  "bs" 'create-scratch-buffer
  "br" 'revert-buffer
)

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

(leader-set-keys
  "c" '(:ignore t :wk "comment")
  "cl" 'comment-or-uncomment-region-or-line
)

(leader-set-keys-for-major-mode 'csv-mode "a" 'csv-align-mode)
(leader-set-keys-for-major-mode 'csv-mode "s" 'csv-set-separator)

(leader-set-keys
  "a" '(:ignore t :wk "applications")
  "ad" 'dired-jump
)

(leader-set-keys
  "e" '(:ignore t :wk "eval")
  "es" 'eval-last-sexp
)

(defun mk/find-user-init-file ()
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/config.org")))

(leader-set-keys
  "f" '(:ignore t :wk "files")
  "ff" 'ido-find-file
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
  "h" '(:ignore t :wk "help")
  "hv" 'helpful-variable
  "hk" 'helpful-key
  "hf" 'helpful-function
  "ht" 'helpful-at-point
)

(leader-set-keys
  "j" '(:ignore t :wk "jump")
  "ji" 'consult-imenu
  "jI" 'consult-eglot
)

(leader-set-keys-for-major-mode 'latex-mode "c" 'latex/build)
(leader-set-keys-for-major-mode 'latex-mode "b" 'TeX-command-master)
(leader-set-keys-for-major-mode 'latex-mode "v" 'TeX-view)
(leader-set-keys-for-major-mode 'latex-mode "m" 'TeX-insert-macro)
(leader-set-keys-for-major-mode 'latex-mode "e" 'LaTeX-environment)
(leader-set-keys-for-major-mode 'latex-mode "l" 'TeX-error-overview)
(leader-set-keys-for-major-mode 'latex-mode "-" 'TeX-recenter-output-buffer)
(leader-set-keys-for-major-mode 'latex-mode "r" 'consult-reftex-insert-reference)
(leader-set-keys-for-major-mode 'latex-mode "s" 'LaTeX-section)
(leader-set-keys-for-major-mode 'latex-mode "C" 'citar-insert-citation)
(leader-set-keys-for-major-mode 'latex-mode "R" 'reftex-toc)
(leader-set-keys-for-major-mode 'latex-mode "xb" 'latex/font-bold)
(leader-set-keys-for-major-mode 'latex-mode "xe" 'latex/font-emphasis)
(leader-set-keys-for-major-mode 'latex-mode "xi" 'latex/font-italic)
(leader-set-keys-for-major-mode 'latex-mode "xc" 'latex/font-code)
(leader-set-keys-for-major-mode 'latex-mode "xs" 'latex/font-small-caps)
(leader-set-keys-for-major-mode 'latex-mode "t" 'mk/align-latex-table)

(leader-set-keys
  "K" '(:ignore t :wk "macros")
  "K" 'kmacro-call-macro
)

(leader-set-keys-for-major-mode 'python-mode "c" 'compile)
(leader-set-keys-for-major-mode 'python-mode "=" 'yapfify-buffer)

(leader-set-keys-for-major-mode 'ess-r-mode "c" 'ess-eval-buffer)

(leader-set-keys-for-major-mode 'shell-mode "h" 'consult-history)

(leader-set-keys
  "S" '(:ignore t :wk "Spelling")
  "Sb" 'flyspell-buffer
  "Sc" 'flyspell-correct-at-point
  "SC" 'consult-flyspell
  "Sn" 'flyspell-goto-next-error
  "Sd" 'ispell-change-dictionary
)

(leader-set-keys
  "s" '(:ignore t :wk "subsitute")
  "se" '(evil-iedit-state/iedit-mode)
  "sr" 'sp-rewrap-sexp
  "sd" 'sp-splice-sexp
)

(leader-set-keys
  "x" '(:ignore t :wk "text")
  "xp" 'fill-paragraph
  "xP" 'unfill-paragraph
  "xC" 'capitalize-word
  "xL" 'downcase-word
  "xT" 'titlecase-region
  "xa" 'mk/convert-to-umlaut
  "xi" 'hydra-transient-special-characters/body
)



(leader-set-keys
  "t" '(:ignore t :wk "toggles")
  "ta" 'auto-fill-mode
  "tl" 'toggle-truncate-lines
  "tL" 'display-line-numbers-mode
)

(leader-set-keys
  "u" 'universal-argument
)

(defhydra hydra-transient-window-resize (:timeout 4)
  "resize window cyclically"
  ("+" mk/enlarge-window "enlarge window")
  ("-" mk/shrink-window "shrink window")
  ("=" balance-windows "balance windows")
  ("s" cycle-resize-window-vertically "resize vertically")
  ("v" cycle-resize-window-horizontally "resize horizontally"))

(leader-set-keys
  "w" '(:ignore t :wk "window")
  "wd" 'delete-window
  "wD" 'delete-other-windows
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
  "wt" 'mk/split-to-shell
  "wT" 'mk/split-to-shell-fullscreen
  "wr" 'hydra-transient-window-resize/body
  "wc" 'writeroom-mode
)

(leader-set-keys
  "z" '(:ignore t :wk "zoom")
  "zx" 'text-scale-adjust
)
