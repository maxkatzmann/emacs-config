;; Disable toolbar.
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Line numbers
(add-hook 'prog-mode-hook (lambda () (setq display-line-numbers 'relative)))
(add-hook 'text-mode-hook (lambda () (setq display-line-numbers 'relative)))

;; Line spacing
(setq-default line-spacing 2)
;; Adjust line spacing.
(setq default-text-properties '(line-spacing 0.2 line-height 1.2))

;; Disable line-wrap by default.
(add-hook 'hack-local-variables-hook (lambda () (setq truncate-lines t)))

;; Highlight current line.
(global-hl-line-mode 1)

;; Use SVG Tags
(straight-use-package 'svg-tag-mode)
(require 'svg-tag-mode)
(setq svg-tag-tags
      '(("\\todo" . ((lambda (tag)
                       (svg-tag-make "TODO" :radius 3 :inverse t :font-size 14.0))))
        ("TODO" . ((lambda (tag)
                       (svg-tag-make "TODO" :radius 3 :inverse t :font-size 14.0))))))
(add-hook 'prog-mode-hook (lambda ()
                            (svg-tag-mode t)))
(add-hook 'text-mode-hook (lambda ()
                            (svg-tag-mode t)))
