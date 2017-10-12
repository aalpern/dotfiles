;;; aalpern-light -- A dark background color theme.
;;
;;; Commentary:
;;
;;; Code:

(deftheme aalpern-light)

(let ((background  "gray97")
      (foreground  "black")
      (comment     "firebrick"))

  (custom-theme-set-faces
   'aalpern-light

   `(default      ((t (:foreground ,foreground :background ,background :inherit nil))))
   `(fringe       ((t (:background ,background))))
   `(cursor       ((t (:background "red"))))
   `(highlight    ((t (:foreground "black" :background "DarkRed"))))
   `(region       ((t (:foreground "white" :background "DarkRed"))))
   `(button       ((t (:inherit (link)))))
   `(link         ((t (:underline (:color foreground-color :style line) :foreground "orange"))))
   `(link-visited ((t (:foreground "OrangeRed" :underline (:color foreground-color :style line)))))

   `(trailing-whitespace ((((class color) (background light)) (:background "OrangeRed3"))
                          (((class color) (background dark)) (:background "OrangeRed3"))
                          (t (:inverse-video t))))

   ;; font-lock
   `(font-lock-builtin-face ((t (:foreground "blue"))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,comment))))
   `(font-lock-comment-face ((t (:foreground ,comment))))
   `(font-lock-doc-face ((t (:foreground ,comment))))
   `(font-lock-constant-face ((t (:foreground "blue"))))
   `(font-lock-function-name-face ((t (:foreground "blue"))))
   `(font-lock-keyword-face ((t (:weight bold :foreground "purple"))))
   `(font-lock-negation-char-face ((t (nil nil))))
   `(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground "red"))))
   `(font-lock-regexp-grouping-construct ((t (:foreground "red"))))
   `(font-lock-string-face ((t (:foreground "sienna"))))
   `(font-lock-type-face ((t (:foreground "DarkOliveGreen"))))
   `(font-lock-variable-name-face ((t (:foreground "DarkGoldenrod"))))
   `(font-lock-warning-face ((t (:inherit (error)))))
   `(font-lock-reference-face ((t (:foreground "red"))))

   ;; powerline
   `(powerline-active1 ((t (:foreground "white" :background "gray20"))))
   `(powerline-active2 ((t (:foreground "gray90" :background "gray40"))))
   `(powerline-inactive1 ((t (:foreground "gray70" :background "gray20"))))
   `(powerline-inactive2 ((t (:foreground "gray60" :background "gray40"))))
   `(mode-line ((t (:foreground "white" :background "firebrick" :box nil))))

   ;; git-gutter
   `(git-gutter+-added ((t (:foreground "DarkOliveGreen" :background "DarkOliveGreen"))))
   `(git-gutter+-modified ((t (:foreground "purple4" :background "purple4"))))
   `(git-gutter+-deleted ((t (:foreground "red3" :background "red3"))))

   ;; web-mode
   `(web-mode-html-tag-face ((t (:foreground "blue" :weight bold))))
   `(web-mode-html-tag-bracket-face ((t (:foreground "blue" :weight bold))))
   `(web-mode-html-attr-name-face ((t (:foreground "DarkRed"))))

   ))

(provide-theme 'aalpern-light)

;;; aalpern-light-theme.el ends here
