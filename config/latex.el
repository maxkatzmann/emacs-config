(require 'cl-lib)

(straight-use-package 'auctex)
(straight-use-package 'ivy-bibtex)

;; TeX Distribution
(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin/"))
(setq exec-path (append exec-path '("/Library/TeX/texbin/")))

;; Open error overview after build.
(setq TeX-error-overview-open-after-TeX-run t)

;; Synctex
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-start-server t)
(setq TeX-source-correlate-method 'synctex)

;; PDF Output
(setq TeX-view-program-list
      '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
(setq TeX-view-program-selection '((output-pdf "Skim")))

;; Smaller fill-column in latex mode.
(add-hook 'TeX-mode-hook (lambda ()
                           (lsp-ui-doc-mode -1)
                           (setq fill-column 70)))

;; Bibtex
(straight-use-package 'org-ref)

;; Bibtex key generation.
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

;; Sentences should end with double spaces.
(setq sentence-end-double-space t)

;; Turn on RefTeX in AUCTeX
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; Activate nice interface between RefTeX and AUCTeX
(setq reftex-plug-into-AUCTeX t)

(setq reftex-external-file-finders
      '(("tex" . "kpsewhich -format=.tex %f")
        ("bib" . "kpsewhich -format=.bib %f")))

(setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))

;; LatexMk
(straight-use-package 'auctex-latexmk)
(setq auctex-latexmk-inherit-TeX-PDF-mode t)
(auctex-latexmk-setup)
(setq TeX-command-default "LatexMk")
(setq latex-build-command "LatexMk")

;; Build command that uses latexmk.
(defun latex/build ()
  (interactive)
  (progn
    (let ((TeX-save-query nil))
      (TeX-save-document (TeX-master-file)))
    (TeX-command latex-build-command 'TeX-master-file -1)))

;; Font changes
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

