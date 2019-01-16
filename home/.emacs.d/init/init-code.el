;;
;;; Code Editing (Git and Dash)
;;

(require 'git-gutter+)

;; Uncomment these lines for the fringe version, which can be used
;; with linenum mode or put on the right side of the buffer.
;; (require 'fringe-helper)
;; (require 'git-gutter-fringe+)

(global-git-gutter+-mode t)

(case system-type
  ('darwin
   (progn
     (require 'dash-at-point)
     (global-set-key "\C-cd" 'dash-at-point))))

;;
;;; Whitespace hygiene
;;

;; (require 'highlight-chars)
;; (add-hook 'font-lock-mode-hook 'hc-highlight-tabs)
;; (hc-toggle-highlight-trailing-whitespace)
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;;
;;; Highlighting
;;

(setq-default font-lock-auto-fontify  t
              font-lock-use-fonts     t
              font-lock-use-colors    t
              font-lock-use-maximal-decoration  t
              font-lock-mode-disable-list       nil)
(require 'font-lock)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size nil)

;;
;;; Symbol Styling
;;

(require 'string-inflection)
(global-set-key (kbd "C-c C-i") 'string-inflection-cycle)
(global-set-key (kbd "C-c C") 'string-inflection-camelcase)        ;; Force to CamelCase
(global-set-key (kbd "C-c L") 'string-inflection-lower-camelcase)  ;; Force to lowerCamelCase

(provide 'init-code)
