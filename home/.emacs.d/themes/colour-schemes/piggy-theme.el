(deftheme piggy "Piggy")

(custom-theme-set-faces
  'piggy
  '(default ((t (
                 :inverse-video nil
                 :foreground "#EDEBE6"
                 :background "#1c1618"
                 :stipple nil
                 :inherit nil))))
'(cursor ((t (:background "#f8f8f0"))))
'(escape-glyph ((t (:foreground "brown"))))
'(minibuffer-prompt ((t (:foreground "#F52E62"))))
'(highlight ((t (:background "#ffe792" :foreground "#000000"))))
'(region ((t (:background "#F52E62"
              :foreground "#ffffff"))))
'(shadow ((t (:foreground "#3b3a32"))))
'(secondary-selection ((t (:background "#F52E62"))))
'(font-lock-builtin-face ((t (:foreground "#F52E62"))))
'(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
'(font-lock-comment-face ((t (:foreground "#3f3236"))))
'(font-lock-constant-face ((t (:foreground "#FD6A5D"))))
'(font-lock-doc-face ((t (:foreground "#6a515a"))))
'(font-lock-function-name-face ((t (:foreground "#FF5D80"))))
'(font-lock-keyword-face ((t (:foreground "#F52E62"))))
'(font-lock-negation-char-face ((t nil)))
'(font-lock-preprocessor-face ((t (:foreground "#F52E62"))))
'(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
'(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
'(font-lock-string-face ((t (:foreground "#FF453E"))))
'(font-lock-type-face ((t (:inherit 'default))))
'(font-lock-variable-name-face ((t (:foreground "#F52E62"))))
'(font-lock-warning-face ((t (:background "#00a8c6" :foreground "#f8f8f0"))))
'(button ((t (:inherit (link)))))
'(link ((((class color) (min-colors 88) (background light)) (:underline (:color foreground-color :style line) :foreground "RoyalBlue3")) (((class color) (background light)) (:underline (:color foreground-color :style line) :foreground "blue")) (((class color) (min-colors 88) (background dark)) (:underline (:color foreground-color :style line) :foreground "cyan1")) (((class color) (background dark)) (:underline (:color foreground-color :style line) :foreground "cyan")) (t (:inherit (underline)))))
'(link-visited ((default (:inherit (link))) (((class color) (background light)) (:foreground "magenta4")) (((class color) (background dark)) (:foreground "violet"))))
'(fringe ((((class color) (background light)) (:background "grey95")) (((class color) (background dark)) (:background "grey10")) (t (:background "gray"))))
'(header-line ((default (:inherit (mode-line))) (((type tty)) (:underline (:color foreground-color :style line) :inverse-video nil)) (((class color grayscale) (background light)) (:box nil :foreground "grey20" :background "grey90")) (((class color grayscale) (background dark)) (:box nil :foreground "grey90" :background "grey20")) (((class mono) (background light)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "black" :background "white")) (((class mono) (background dark)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "white" :background "black"))))
'(tooltip ((((class color)) (:inherit (variable-pitch) :foreground "black" :background "lightyellow")) (t (:inherit (variable-pitch)))))
'(mode-line ((((class color) (min-colors 88)) (:foreground "black" :background "grey75" :box (:line-width -1 :color nil :style released-button))) (t (:inverse-video t))))
'(mode-line-buffer-id ((t (:weight bold))))
'(mode-line-emphasis ((t (:weight bold))))
'(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
'(mode-line-inactive ((default (:inherit (mode-line))) (((class color) (min-colors 88) (background light)) (:background "grey90" :foreground "grey20" :box (:line-width -1 :color "grey75" :style nil) :weight light)) (((class color) (min-colors 88) (background dark)) (:background "grey30" :foreground "grey80" :box (:line-width -1 :color "grey40" :style nil) :weight light))))
'(isearch ((t (:background "#ffe792" :foreground "#000000"))))
'(isearch-fail ((t (:background "#00a8c6" :foreground "#f8f8f0"))))
'(lazy-highlight ((t (:background "#ffe792" :foreground "#000000"))))
'(match ((t (:background "#ffe792" :foreground "#000000"))))
'(next-error ((t (:inherit (region)))))
'(query-replace ((t (:inherit (isearch)))))
)

(provide-theme 'piggy )
