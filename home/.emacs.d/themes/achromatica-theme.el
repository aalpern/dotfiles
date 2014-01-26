(deftheme achromatica)

(custom-theme-set-faces
 'achromatica

 '(default ((t (:foreground "gray85" :background "gray13" :inherit nil))))
 '(fringe ((t (:background "gray5"))))
 '(cursor ((t (:background "red"))))
 '(highlight ((t (:foreground "gray65" :background "gray25"))))
 '(region ((t (:foreground "black" :background "gray50"))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:underline (:color foreground-color :style line) :foreground "firebrick3"))))
 '(link-visited ((t (:foreground "firebrick4" :underline (:color foreground-color :style line)))))

 '(trailing-whitespace ((((class color) (background light)) (:background "gray35")) (((class color) (background dark)) (:background "gray35")) (t (:inverse-video t))))

 ;; font-lock
 '(font-lock-builtin-face ((t (:foreground "gray44"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "gray33"))))
 '(font-lock-comment-face ((t (:foreground "gray33"))))
 '(font-lock-doc-face ((t (:foreground "gray33"))))
 '(font-lock-constant-face ((t (:foreground "white"))))
 '(font-lock-function-name-face ((t (:foreground "white"))))
 '(font-lock-keyword-face ((t (:foreground "white"))))
 '(font-lock-negation-char-face ((t (nil nil))))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "gray75"))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "gray75"))))
 '(font-lock-string-face ((t (:foreground "gray66"))))
 '(font-lock-type-face ((t (:foreground "white"))))
 '(font-lock-variable-name-face ((t (:foreground "white"))))
 '(font-lock-warning-face ((t (:foreground "red4"))))

 ;; powerline
 '(powerline-active1 ((t (:foreground "black" :background "gray50"))))
 '(powerline-active2 ((t (:foreground "OrangeRed" :background "gray20"))))
 '(powerline-inactive1 ((t (:foreground "gray70" :background "#002200"))))
 '(powerline-inactive2 ((t (:foreground "gray60" :background "gray20"))))
 '(mode-line ((t (:foreground "black" :background "firebrick4" :box nil))))

 ;; git-gutter
 '(git-gutter+-added ((t (:foreground "green4" :background "green4"))))
 '(git-gutter+-modified ((t (:foreground "purple" :background "purple"))))
 '(git-gutter+-deleted ((t (:foreground "red3" :background "red3"))))
 )

(provide-theme 'achromatica)
