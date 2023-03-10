
;; If you are distributing this theme, please replace this comment
;; with the appropriate license attributing the original VS Code
;; theme author.

(deftheme github-light "A nice  theme.")


(let (
(color0 "#fff")
(color1 "#24292e")
(color2 "#ebebeb")
(color3 "#044289")
(color4 "#d2d2d2")
(color5 "#e6e6e6")
(color6 "#444c55")
(color7 "#d7d7d7")
(color8 "#303841")
(color9 "#2f363d")
(color10 "#b4b4b4")
(color11 "#6a737d")
(color12 "#005cc5")
(color13 "#d73a49")
(color14 "#032f62")
(color15 "#e36209")
(color16 "#888d92")
(color17 "#f0f0f0")
(color18 "#151a1f")
(color19 "#f4f4f4")
(color20 "#191e23"))


(custom-theme-set-faces
'github-light


;; BASIC FACES
`(default ((t (:background ,color0 :foreground ,color1 ))))
`(hl-line ((t (:background ,color2 ))))
`(cursor ((t (:foreground ,color3 ))))
`(region ((t (:background ,color4 ))))
`(secondary-selection ((t (:background ,color5 ))))
`(fringe ((t (:background ,color0 ))))
`(mode-line-inactive ((t (:background ,color2 :foreground ,color6 ))))
`(mode-line ((t (:background ,color7 :foreground ,color8 ))))
`(minibuffer-prompt ((t (:background ,color0 :foreground ,color9 ))))
`(vertical-border ((t (:foreground ,color10 ))))


;; FONT LOCK FACES
`(font-lock-comment-face ((t (:foreground ,color11 ))))
`(font-lock-constant-face ((t (:foreground ,color12 ))))
`(font-lock-keyword-face ((t (:foreground ,color13 ))))
`(font-lock-string-face ((t (:foreground ,color14 ))))
`(font-lock-variable-name-face ((t (:foreground ,color15 ))))


;; linum-mode
`(linum ((t (:foreground ,color16 ))))
`(linum-relative-current-face ((t (:foreground ,color16 ))))


;; display-line-number-mode
`(line-number ((t (:foreground ,color16 ))))
`(line-number-current-line ((t (:foreground ,color16 ))))


;; THIRD PARTY PACKAGE FACES


;; doom-modeline-mode
`(doom-modeline-bar ((t (:background ,color7 :foreground ,color8 ))))
`(doom-modeline-inactive-bar ((t (:background ,color2 :foreground ,color6 ))))


;; web-mode
`(web-mode-string-face ((t (:foreground ,color14 ))))
`(web-mode-html-tag-face ((t (:foreground ,color13 ))))
`(web-mode-html-tag-bracket-face ((t (:foreground ,color13 ))))


;; company-mode
`(company-tooltip ((t (:background ,color17 :foreground ,color18 ))))


;; org-mode
`(org-block ((t (:background ,color19 :foreground ,color20 ))))
`(org-block-begin-line ((t (:foreground ,color11 ))))))


(custom-theme-set-variables
  'github-light
  '(linum-format " %3i "))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))


;;;###autoload
(defun github-light-theme()
  "Apply the github-light-theme."
  (interactive)
  (load-theme 'github-light t))


(provide-theme 'github-light)


;; Local Variables:
;; no-byte-compile: t
;; End:


;; Generated using https://github.com/nice/themeforge
;; Feel free to remove the above URL and this line.
