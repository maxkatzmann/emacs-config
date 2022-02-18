(require 'nano-theme-light)
(require 'nano-theme-dark)

;; ;; Fira Code
;; (straight-use-package 'fira-code-mode)
;; (global-fira-code-mode)
;; ;; Don't forget to execute `fira-code-mode-install-fonts`!
;; (setq fira-code-mode-disabled-ligatures '("x" ":" "..." ".." "%%" "::"))

;; change the font
(setq nano-font-family-monospaced "Roboto Mono")
;; (setq nano-font-family-monospaced "Fira Code")

;; nano disables font-lock, which we reenable because it's nice.
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size 256000)

;; nano disables the fringe. We want it back.
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
                 (setq nano-theme-var "dark")))
          ((string= nano-theme-var "dark")
          (progn (nano-theme-set-light)
                 (nano-refresh-theme)
                 (setq nano-theme-var "light")))
          (t nil)))

(nano-theme-set-light)
(nano-refresh-theme)
