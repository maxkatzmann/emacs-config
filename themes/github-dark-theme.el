
;; If you are distributing this theme, please replace this comment
;; with the appropriate license attributing the original VS Code
;; theme author.

(deftheme github-dark "A nice  theme.")


(let (
(color0 "#24292e")
(color1 "#e1e4e8")
(color2 "#10151a")
(color3 "#c8e1ff")
(color4 "#000001")
(color5 "#0b1015")
(color6 "#bdc1c6")
(color7 "#000106")
(color8 "#a9adb2")
(color9 "#000000")
(color10 "#6a737d")
(color11 "#79b8ff")
(color12 "#f97583")
(color13 "#9ecbff")
(color14 "#ffab70")
(color15 "#444d56")
(color16 "#151a1f")
(color17 "#d2d5d9")
(color18 "#191e23")
(color19 "#d6d9dd"))


(custom-theme-set-faces
'github-dark


;; BASIC FACES
`(default ((t (:background ,color0 :foreground ,color1 ))))
`(hl-line ((t (:background ,color15 ))))
`(cursor ((t (:foreground ,color3 ))))
`(region ((t (:background ,color4 ))))
`(secondary-selection ((t (:background ,color5 ))))
`(fringe ((t (:background ,color0 ))))
`(mode-line-inactive ((t (:background ,color2 :foreground ,color6 ))))
`(mode-line ((t (:background ,color7 :foreground ,color8 ))))
`(minibuffer-prompt ((t (:background ,color0 :foreground ,color1 ))))
`(vertical-border ((t (:foreground ,color9 ))))


;; FONT LOCK FACES
`(font-lock-comment-face ((t (:foreground ,color10 ))))
`(font-lock-constant-face ((t (:foreground ,color11 ))))
`(font-lock-keyword-face ((t (:foreground ,color12 ))))
`(font-lock-string-face ((t (:foreground ,color13 ))))
`(font-lock-variable-name-face ((t (:foreground ,color14 ))))


;; linum-mode
`(linum ((t (:foreground ,color15 ))))
`(linum-relative-current-face ((t (:foreground ,color15 ))))


;; display-line-number-mode
`(line-number ((t (:foreground ,color15 ))))
`(line-number-current-line ((t (:foreground ,color15 ))))


;; THIRD PARTY PACKAGE FACES


;; doom-modeline-mode
`(doom-modeline-bar ((t (:background ,color7 :foreground ,color8 ))))
`(doom-modeline-inactive-bar ((t (:background ,color2 :foreground ,color6 ))))


;; web-mode
`(web-mode-string-face ((t (:foreground ,color13 ))))
`(web-mode-html-tag-face ((t (:foreground ,color12 ))))
`(web-mode-html-tag-bracket-face ((t (:foreground ,color12 ))))


;; company-mode
`(company-tooltip ((t (:background ,color16 :foreground ,color17 ))))


;; org-mode
`(org-block ((t (:background ,color18 :foreground ,color19 ))))
`(org-block-begin-line ((t (:foreground ,color10 ))))))


(custom-theme-set-variables
  'github-dark
  '(linum-format " %3i "))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))


;;;###autoload
(defun github-dark-theme()
  "Apply the github-dark-theme."
  (interactive)
  (load-theme 'github-dark t))


(provide-theme 'github-dark)


;; Local Variables:
;; no-byte-compile: t
;; End:


;; Generated using https://github.com/nice/themeforge
;; Feel free to remove the above URL and this line.
