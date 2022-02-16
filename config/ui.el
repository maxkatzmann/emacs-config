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

(setq svg-tag-tags
      `(
        ;; Org tags
        (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
        (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))
        
        ;; Task priority
        ("\\[#[A-Z]\\]" . ( (lambda (tag)
                              (svg-tag-make tag :face 'org-priority 
                                            :beg 2 :end -1 :margin 0))))

        ;; Progress
        ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                            (svg-progress-percent (substring tag 1 -2)))))
        ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                          (svg-progress-count (substring tag 1 -1)))))
        
        ;; TODO / DONE
        ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :font-size 14.0 :margin 0))))
        ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :font-size 14.0 :margin 0))))
        ("\\todo" . ((lambda (tag) (svg-tag-make "TODO" :radius 3 :inverse t :font-size 14.0))))


        ;; Citation of the form [cite:@Knuth:1984] 
        ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                          (svg-tag-make tag
                                                        :inverse t
                                                        :beg 7 :end -1
                                                        :crop-right t))))
        ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                (svg-tag-make tag
                                                              :end -1
                                                              :crop-left t))))

        
        ;; Active date (without day name, with or without time)
        (,(format "\\(<%s>\\)" date-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :end -1 :font-size 14.0 :margin 0))))
        (,(format "\\(<%s %s *\\)%s>" date-re day-re time-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :inverse nil :crop-right t :font-size 14.0 :margin 0))))
        (,(format "<%s %s *\\(%s>\\)" date-re day-re time-re) .
         ((lambda (tag)
            (svg-tag-make tag :end -1 :inverse t :crop-left t :font-size 14.0 :margin 0))))

        ;; Inactive date  (without day name, with or without time)
         (,(format "\\(\\[%s\\]\\)" date-re) .
          ((lambda (tag)
             (svg-tag-make tag :beg 1 :end -1 :font-size 14.0 :margin 0 :face 'org-date))))
         (,(format "\\(\\[%s %s *\\)%s\\]" date-re day-re time-re) .
          ((lambda (tag)
             (svg-tag-make tag :beg 1 :inverse nil :crop-right t :font-size 14.0 :margin 0 :face 'org-date))))
         (,(format "\\[%s %s *\\(%s\\]\\)" date-re day-re time-re) .
          ((lambda (tag)
             (svg-tag-make tag :end -1 :inverse t :crop-left t :font-size 14.0 :margin 0 :face 'org-date)))))) 

(add-hook 'prog-mode-hook (lambda ()
                            (svg-tag-mode t)))
(add-hook 'text-mode-hook (lambda ()
                            (svg-tag-mode t)))
