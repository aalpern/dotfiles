;;; better-github -- A more faithful rendition of the github colorizing scheme
;;
;;; Commentary:
;;
;;; Code:

(deftheme better-github)

(let ((background  "white")
      (background2 "gray15")
      (foreground  "black")

      (olivegreen  "DarkOliveGreen")
      (darkgreen   "green4")

      (dark-blue "#445588")
      (dark-red "#990000")
      (medium-gray "#555555")
      (light-gray "#999988")
      (lighter-gray "#BBBBAA")
      (pink "#dd1144")
      (teal "#008080")

      (comment "red")
)

  (custom-theme-set-faces
   'better-github

   `(default ((t (:foreground ,foreground :background ,background :inherit nil))))
   `(fringe ((t (:background ,background))))
   `(cursor ((t (:background "red"))))
   `(highlight ((t (:foreground "white" :background ,pink))))
   `(region ((t (:foreground "black" :background "wheat"))))
   `(button ((t (:inherit (link)))))
   `(link ((t (:underline (:color foreground-color :style line) :foreground "DeepPink"))))
   `(link-visited ((t (:foreground "Purple" :underline (:color foreground-color :style line)))))

   `(trailing-whitespace ((((class color) (background light)) (:background "red"))
                          (((class color) (background dark)) (:background "red"))
                          (t (:inverse-video t))))

   ;; font-lock
   `(font-lock-builtin-face ((t (:foreground ,dark-red :weight bold))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,comment :italic t))))
   `(font-lock-comment-face ((t (:foreground ,comment :italic t))))
   `(font-lock-doc-face ((t (:foreground ,comment :italic t))))
   `(font-lock-constant-face ((t (:foreground ,medium-gray))))
   `(font-lock-function-name-face ((t (:foreground ,dark-red :weight bold))))
   `(font-lock-keyword-face ((t (:foreground ,foreground :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,pink :weight bold))))
   `(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,pink))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,pink))))
   `(font-lock-string-face ((t (:foreground ,pink))))
   `(font-lock-type-face ((t (:foreground ,teal))))
   `(font-lock-variable-name-face ((t (:foreground ,teal))))
   `(font-lock-warning-face ((t (:inherit (error)))))
   `(c-annotation-face ((t (:foreground ,foreground))))

   ;; powerline
   `(powerline-active1 ((t (:foreground "white" :background "gray20"))))
   `(powerline-active2 ((t (:foreground ,foreground :background ,lighter-gray))))
   `(powerline-inactive1 ((t (:foreground "gray70" :background "gray20"))))
   `(powerline-inactive2 ((t (:foreground "gray60" :background "gray40"))))
   `(mode-line ((t (:foreground "white" :background ,pink :box nil))))

   ;; git-gutter
   `(git-gutter+-added ((t (:foreground ,darkgreen :background ,darkgreen))))
   `(git-gutter+-modified ((t (:foreground "purple4" :background "purple4"))))
   `(git-gutter+-deleted ((t (:foreground "red3" :background "red3"))))

   ;; web-mode
   `(web-mode-html-tag-face ((t (:weight bold))))
   `(web-mode-html-tag-bracket-face ((t (:weight bold))))
   `(web-mode-html-attr-name-face ((t (:foreground ,dark-blue))))
   ))

(provide-theme 'better-github)

;;; better-github-theme.el ends here
