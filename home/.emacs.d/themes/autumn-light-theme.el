;;; autumn-light -- A light color theme with muted, autumnal colors.
;;
;;; Commentary:
;;
;;; Code:

(deftheme autumn-light)

(custom-theme-set-faces
 'autumn-light

 '(default ((t (:foreground "black" :background "wheat" :inherit nil))))
 '(fringe ((t (:background "wheat"))))
 '(cursor ((t (:background "red"))))
 '(highlight ((t (:foreground "white" :background "DarkSlateBlue"))))
 '(region ((t (:foreground "gray90" :background "DarkSlateBlue"))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:underline (:color foreground-color :style line) :foreground "DarkSlateBlue"))))
 '(link-visited ((t (:foreground "Purple" :underline (:color foreground-color :style line)))))

 '(trailing-whitespace ((((class color) (background light)) (:background "red1")) (((class color) (background dark)) (:background "red1")) (t (:inverse-video t))))

 ;; font-lock
 '(font-lock-builtin-face ((t (:foreground "MediumPurple4"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "firebrick4"))))
 '(font-lock-comment-face ((t (:foreground "firebrick4"))))
 '(font-lock-doc-face ((t (:foreground "firebrick4"))))
 '(font-lock-constant-face ((t (:foreground "blue4"))))
 '(font-lock-function-name-face ((t (:foreground "MediumBlue"))))
 '(font-lock-keyword-face ((t (:foreground "DarkOrchid4"))))
 '(font-lock-negation-char-face ((t (nil nil))))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "DarkOliveGreen"))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "DarkOliveGreen"))))
 '(font-lock-string-face ((t (:foreground "gray30"))))
 '(font-lock-type-face ((t (:foreground "purple4"))))
 '(font-lock-variable-name-face ((t (:foreground "DarkGreen"))))
 '(font-lock-warning-face ((t (:foreground "red"))))

 ;; powerline
 '(powerline-active1 ((t (:foreground "white" :background "gray20"))))
 '(powerline-active2 ((t (:foreground "gray90" :background "gray40"))))
 '(powerline-inactive1 ((t (:foreground "gray70" :background "gray20"))))
 '(powerline-inactive2 ((t (:foreground "gray60" :background "gray40"))))
 '(mode-line ((t (:foreground "white" :background "firebrick" :box nil))))

 ;; git-gutter
 '(git-gutter+-added ((t (:foreground "green4" :background "green4"))))
 '(git-gutter+-modified ((t (:foreground "purple" :background "purple"))))
 '(git-gutter+-deleted ((t (:foreground "red3" :background "red3"))))
 )

(provide-theme 'autumn-light)

;;; autumn-light-theme.el ends here
