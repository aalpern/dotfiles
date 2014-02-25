(deftheme aalpern-dark2)

(custom-theme-set-faces
 'aalpern-dark2

 '(default ((t (:foreground "papaya whip" :background "gray15" :inherit nil))))
 '(fringe ((t (:background "gray15"))))
 '(cursor ((t (:background "red"))))
 '(highlight ((t (:foreground "white" :background "darkslateblue"))))
 '(region ((t (:foreground "black" :background "wheat"))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:underline (:color foreground-color :style line) :foreground "DarkSlateBlue"))))
 '(link-visited ((t (:foreground "Purple" :underline (:color foreground-color :style line)))))

 '(trailing-whitespace ((((class color) (background light)) (:background "red1")) (((class color) (background dark)) (:background "red1")) (t (:inverse-video t))))

 ;; font-lock
 '(font-lock-builtin-face ((t (nil nil))))
 '(font-lock-comment-delimiter-face ((t (:foreground "OrangeRed"))))
 '(font-lock-comment-face ((t (:slant italic :foreground "OrangeRed"))))
 '(font-lock-doc-face ((t (:foreground "OrangeRed"))))
 '(font-lock-constant-face ((t (:foreground "aquamarine"))))
 '(font-lock-function-name-face ((t (:foreground "LightSkyBlue"))))
 '(font-lock-keyword-face ((t (:weight bold :foreground "LightSteelBlue"))))
 '(font-lock-negation-char-face ((t (nil nil))))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "DarkOliveGreen"))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "DarkOliveGreen"))))
 '(font-lock-string-face ((t (:foreground "LightSalmon"))))
 '(font-lock-type-face ((t (:foreground "PaleGreen"))))
 '(font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))
 '(font-lock-warning-face ((t (:inherit (error)))))

 ;; powerline
 '(powerline-active1 ((t (:foreground "white" :background "gray20"))))
 '(powerline-active2 ((t (:foreground "gray90" :background "gray40"))))
 '(powerline-inactive1 ((t (:foreground "gray70" :background "gray20"))))
 '(powerline-inactive2 ((t (:foreground "gray60" :background "gray40"))))
 '(mode-line ((t (:foreground "white" :background "firebrick" :box nil))))

 ;; git-gutter
 '(git-gutter+-added ((t (:foreground "green4" :background "green4"))))
 '(git-gutter+-modified ((t (:foreground "purple4" :background "purple4"))))
 '(git-gutter+-deleted ((t (:foreground "red3" :background "red3"))))
 )

(provide-theme 'aalpern-dark2)
