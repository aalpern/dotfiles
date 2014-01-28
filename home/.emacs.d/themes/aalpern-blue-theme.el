;;; aalpern-blue -- A dark background color theme.
;;
;;; Commentary:
;;
;;; Code:

(deftheme aalpern-blue)

(let ((background  "#000022")
      (foreground  "LightSteelBlue")
      (comment     "#6666CC"))

  (custom-theme-set-faces
   'aalpern-blue

   `(default ((t (:foreground ,foreground :background ,background :inherit nil))))
   `(fringe ((t (:background ,background))))
   `(cursor ((t (:background "red"))))
   `(highlight ((t (:foreground "white" :background "RoyalBlue"))))
   `(region ((t (:foreground "white" :background "RoyalBlue"))))
   `(button ((t (:inherit (link)))))
   `(link ((t (:underline (:color foreground-color :style line) :foreground "DeepPink"))))
   `(link-visited ((t (:foreground "Purple" :underline (:color foreground-color :style line)))))

   `(trailing-whitespace ((((class color) (background light)) (:background "red"))
                          (((class color) (background dark)) (:background "red"))
                          (t (:inverse-video t))))

   ;; font-lock
   `(font-lock-builtin-face ((t (:foreground "cyan"))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,comment))))
   `(font-lock-comment-face ((t (:foreground ,comment))))
   `(font-lock-doc-face ((t (:foreground ,comment))))
   `(font-lock-constant-face ((t (:foreground "aquamarine"))))
   `(font-lock-function-name-face ((t (:foreground "DarkTurquoise"))))
   `(font-lock-keyword-face ((t (:weight bold :foreground "DodgerBlue"))))
   `(font-lock-negation-char-face ((t (nil nil))))
   `(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground "LightBlue"))))
   `(font-lock-regexp-grouping-construct ((t (:foreground "LightBlue"))))
   `(font-lock-string-face ((t (:foreground "CornflowerBlue"))))
   `(font-lock-type-face ((t (:foreground "cyan"))))
   `(font-lock-variable-name-face ((t (:foreground "DeepSkyBlue"))))
   `(font-lock-warning-face ((t (:inherit (error)))))

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
   ))

(provide-theme 'aalpern-blue)

;;; aalpern-blue-theme.el ends here
