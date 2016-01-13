(deftheme green-phosphor)

(custom-theme-set-faces
 'green-phosphor

 ;;'(default ((t (:foreground "LimeGreen" :background "black" :inherit nil))))
 '(default ((t (:foreground "LimeGreen" :background "#001100" :inherit nil))))
 '(fringe ((t (:background "#001100"))))
 '(cursor ((t (:background "red"))))
 '(highlight ((t (:foreground "black" :background "green"))))
 '(region ((t (:foreground "black" :background "LimeGreen"))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:underline (:color foreground-color :style line) :foreground "green"))))
 '(link-visited ((t (:foreground "green4" :underline (:color foreground-color :style line)))))

 '(trailing-whitespace ((((class color) (background light)) (:background "DarkGreen")) (((class color) (background dark)) (:background "DarkGreen")) (t (:inverse-video t))))

 ;; font-lock
 '(font-lock-builtin-face ((t (:foreground "DarkSeaGreen"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "DarkOliveGreen"))))
 '(font-lock-comment-face ((t (:foreground "DarkOliveGreen"))))
 '(font-lock-doc-face ((t (:foreground "DarkOliveGreen"))))
 '(font-lock-constant-face ((t (:foreground "PaleGreen"))))
 '(font-lock-function-name-face ((t (:foreground "lawn green"))))
 '(font-lock-keyword-face ((t (:foreground "PaleGreen"))))
 '(font-lock-negation-char-face ((t (nil nil))))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "green"))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "green"))))
 '(font-lock-string-face ((t (:foreground "PaleGreen"))))
 '(font-lock-type-face ((t (:foreground "olive drab"))))
 '(font-lock-variable-name-face ((t (:foreground "dark khaki"))))
 '(font-lock-warning-face ((t (:foreground "red"))))

 ;; powerline
 '(powerline-active1 ((t (:foreground "green" :background "#005500"))))
 '(powerline-active2 ((t (:foreground "PaleGreen" :background "#003300"))))
 '(powerline-inactive1 ((t (:foreground "gray70" :background "#002200"))))
 '(powerline-inactive2 ((t (:foreground "gray60" :background "#004400"))))
 '(mode-line ((t (:foreground "black" :background "green" :box nil))))

 ;; git-gutter
 '(git-gutter+-added ((t (:foreground "green4" :background "green4"))))
 '(git-gutter+-modified ((t (:foreground "purple" :background "purple"))))
 '(git-gutter+-deleted ((t (:foreground "red3" :background "red3"))))

 ;; web-mode
 '(web-mode-html-tag-face ((t (:foreground "green4" :weight bold))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "green4" :weight bold))))
 '(web-mode-html-attr-name-face ((t (:foreground "lawn green"))))
 )

(provide-theme 'green-phosphor)
