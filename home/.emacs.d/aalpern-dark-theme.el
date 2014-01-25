(deftheme aalpern-dark
  "Created 2014-01-24.")

(custom-theme-set-faces
 'aalpern-dark

 '(default ((t (:foreground "gray95" :background "black" :inherit nil))))
 '(fringe ((t (:background "black"))))
 '(cursor ((t (:background "red"))))


 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((t (:family "Sans Serif"))))


 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((t (:foreground "#445588"))))

 '(highlight ((t (:foreground "white" :background "darkslateblue"))))
 '(region ((t (:foreground "black" :background "wheat"))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(secondary-selection ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 16) (background light)) (:background "yellow")) (((class color) (min-colors 16) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 8)) (:foreground "black" :background "cyanaa")) (t (:inverse-video t))))
 '(trailing-whitespace ((((class color) (background light)) (:background "red1")) (((class color) (background dark)) (:background "red1")) (t (:inverse-video t))))

 '(font-lock-builtin-face ((t (nil nil))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#999988" :slant italic :italic t))))
 '(font-lock-comment-face ((t (:slant italic :foreground "OrangeRed"))))
 '(font-lock-constant-face ((t (:foreground "aquamarine"))))
 '(font-lock-doc-face ((t (:foreground "OrangeRed"))))
 '(font-lock-function-name-face ((t (:foreground "LightSkyBlue"))))
 '(font-lock-keyword-face ((t (:weight bold :foreground "LightSteelBlue"))))
 '(font-lock-negation-char-face ((t (nil nil))))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#009926"))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "#009926"))))
 '(font-lock-string-face ((t (:foreground "LightSalmon"))))
 '(font-lock-type-face ((t (:foreground "PaleGreen"))))
 '(font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))
 '(font-lock-warning-face ((t (:inherit (error)))))

 '(button ((t (:inherit (link)))))

 '(link ((t (:underline (:color foreground-color :style line) :foreground "blue1"))))
 '(link-visited ((t (:foreground "magenta4" :underline (:color foreground-color :style line)))))



 '(header-line ((default (:inherit (mode-line))) (((type tty)) (:underline (:color foreground-color :style line) :inverse-video nil)) (((class color grayscale) (background light)) (:box nil :foreground "grey20" :background "grey90")) (((class color grayscale) (background dark)) (:box nil :foreground "grey90" :background "grey20")) (((class mono) (background light)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "black" :background "white")) (((class mono) (background dark)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "white" :background "black"))))

 '(tooltip ((((class color)) (:inherit (variable-pitch) :foreground "black" :background "lightyellow")) (t (:inherit (variable-pitch)))))

 )

(provide-theme 'aalpern-dark)
