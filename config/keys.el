;; Alt/Option Backspace to delete words.
(setq mac-option-modifier 'alt)
(global-set-key (kbd "A-<backspace>") 'backward-kill-word)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; evil
(use-package evil
  :init
  (setq evil-want-keybinding nil) ;; Required for evil-collection
  (setq evil-want-visual-char-semi-exclusive t)
  :config
  (evil-mode 1)
  ;; Tell evil to use undo-tree
  (evil-set-undo-system 'undo-tree))

;; Allow evil bindins throughout emacs, e.g., also in magit.
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; evil surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
  (evil-define-key 'visual global-map "s" 'evil-surround-region))

;; evil exchange
(use-package evil-exchange
  :config
  (evil-exchange-install))

;; which key
(use-package which-key
  :config
  (which-key-setup-minibuffer)
  (which-key-mode))

;; iedit
(use-package iedit
  :config
  (straight-use-package 'evil-iedit-state)
  (require 'evil-iedit-state))

;; Use swiper for search.
(define-key evil-normal-state-map "/" 'swiper)

;; Hydra for transient states.
(use-package hydra)

;; spacemode
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

;; Universal
(leader-set-keys
  "u" 'universal-argument
)

;; Applications
(leader-set-keys
  "a" '(:ignore t :wk "applications")
  "ad" 'dired
)


;; Buffers
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

;; Files
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file user-init-file))

(leader-set-keys
  "f" '(:ignore t :wk "files")
  "ff" 'counsel-find-file
  "fc" 'copy-file
  "fh" 'find-file-at-point
  "fed" 'find-user-init-file
)

;; Windows
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

(defun split-to-term ()
  (interactive)
  (split-and-follow-horizontally)
  (evil-window-move-very-bottom)
  (unless (get-buffer "*terminal*")
    (term "/bin/zsh"))
  (switch-to-buffer "*terminal*"))

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
  "wt" 'split-to-term
  "wR" 'hydra-transient-window-resize/body
)

;; Zoom
(leader-set-keys
  "z" '(:ignore t :wk "zoom")
  "zx" 'text-scale-adjust
)

;; Git
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

;; Jumping depending on the mode
(add-hook 'lsp-mode-hook (lambda ()
                           (leader-set-keys
                             "mj" 'lsp-ivy-workspace-symbol)))

;; Help
(leader-set-keys
  "h" '(:ignore t :wk "hel")
  "hv" 'helpful-variable
  "hf" 'helpful-function
  "ht" 'helpful-at-point
)

;; Comments
(defun comment-beginning-of-line ()
  (interactive)
  (comment-line 1)
  (previous-line))

(leader-set-keys
  "c" '(:ignore t :wk "comment")
  "cl" 'comment-beginning-of-line
)

;; Spelling
(leader-set-keys
  "S" '(:ignore t :wk "Spelling")
  "Sc" 'flyspell-auto-correct-word
)

;; Toggles 
(leader-set-keys
  "t" '(:ignore t :wk "toggles")
  "ta" 'auto-fill-mode
  "tl" 'toggle-truncate-lines
)

;; Theme 
(leader-set-keys
  "T" '(:ignore t :wk "Theme")
  "Ts" 'nano-toggle-theme
)

;; Text
(leader-set-keys
  "x" '(:ignore t :wk "text")
  "xp" 'fill-paragraph
  "xP" 'unfill-paragraph
  "xC" 'capitalize-word
  "xL" 'downcase-word
  "xT" 'titlecase-region
)
;; Substitution
(leader-set-keys
  "s" '(:ignore t :wk "subsitute")
  "se" '(evil-iedit-state/iedit-mode)
  "sr" 'sp-rewrap-sexp
  "sd" 'sp-splice-sexp
)

;; eval
(leader-set-keys
  "e" '(:ignore t :wk "eval")
  "es" 'eval-last-sexp
)

;; Projectile
(leader-set-keys
  "/" 'projectile-grep
)

;; Org
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

;; TeX
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

;; Bibtex
(leader-set-keys-for-major-mode 'bibtex-mode "s" 'org-ref-sort-bibtex-entry)
(leader-set-keys-for-major-mode 'bibtex-mode "c" 'bibtex-clean-entry)

;; R
(leader-set-keys-for-major-mode 'ess-r-mode "s" 'R)
(leader-set-keys-for-major-mode 'ess-r-mode "c" 'ess-eval-buffer)
(leader-set-keys-for-major-mode 'ess-r-mode "=" 'lsp-format-buffer)

;; Python
(leader-set-keys-for-major-mode 'python-mode "=" 'yapfify-buffer)

;; Bazel
(leader-set-keys-for-major-mode 'bazel-mode "=" 'bazel-buildifier)

;; C++
(leader-set-keys-for-major-mode 'c++-mode "gd" 'lsp-find-definition)
(leader-set-keys-for-major-mode 'c++-mode "=" 'lsp-format-buffer)
