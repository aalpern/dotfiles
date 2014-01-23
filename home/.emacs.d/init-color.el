;; -*- mode:emacs-lisp -*-

(setq font-lock-background-mode 'dark)
(setq font-lock-maximum-size nil)

(setq-default font-lock-auto-fontify  t
          font-lock-use-fonts     nil
          font-lock-use-colors    t
          font-lock-use-maximal-decoration  t
          font-lock-mode-disable-list       nil)

(setq font-lock-maximum-decoration t)

(require 'font-lock)
;;(require 'font-lock-extra)
;; do more highlighting

(setq lisp-font-lock-keywords lisp-font-lock-keywords-2)

;;(setq font-lock-string-face 'italic)
;;(setq java-font-lock-keywords java-font-lock-keywords-1)
(global-font-lock-mode t)

;; (setq font-lock-support-mode 'lazy-lock-mode)

;;----------------------------------------------------------------------
;;; COMMON

;(set-face-foreground 'default "gray95")
;(set-face-background 'default "black")
(set-face-background 'highlight "darkslateblue")
(set-face-foreground 'highlight "white")
;(set-face-foreground 'region "black")
;(set-face-background 'region "wheat")
(set-face-foreground 'secondary-selection "black")

;; (set-face-foreground 'buffer-group-header-face "white")
;; (set-face-foreground 'buffer-group-header-face "darkslateblue")

;(set-face-background 'modeline "wheat")
;(set-face-foreground 'modeline "black")

;(set-background-color "black")
;(set-foreground-color "gray95")

;;(define-key global-map "\C-l" 'recenter-and-color)

;;(defun recenter-and-color ()
;;  (interactive)
;;  (recenter))

(set-cursor-color "red")

(defun ala-background-color (color)
  (set-face-background 'default color)
  ;; (set-face-background 'fringe color)
  (set-background-color color)
  (set-cursor-color "red"))

(defun ala-background-white ()
  (interactive)
  (ala-background-color "white")
  (set-face-foreground 'default "Black")
;  (set-face-foreground 'modeline "White")
;  (set-face-background 'modeline "Black")
  (set-foreground-color "Black"))

(defun ala-background-black ()
  (interactive)
  (ala-background-color "black")
  (set-face-foreground 'default "Gray95")
;  (set-face-foreground 'modeline "Black")
;  (set-face-background 'modeline "Wheat")
  (set-foreground-color "Gray95"))

(defun ala-background-charcoal ()
  (interactive)
  (ala-background-color "Gray15")
  (set-face-foreground 'default "Gray95")
;  (set-face-foreground 'modeline "Black")
;  (set-face-background 'modeline "Gray95")
  (set-foreground-color "Gray95"))

;; (ala-background-charcoal)
;; (ala-background-white)
;; (ala-background-black)

  (set-face-foreground 'font-lock-comment-face       "gray70")

(defun ala-charcoal ()
  (interactive)
  (ala-background-charcoal)
  (ala-background-color "Gray13")
  (set-face-foreground 'region "black")
  (set-face-background 'region "gray50")

  (set-face-foreground 'default "Gray85")

  (set-face-foreground 'font-lock-comment-face       "Gray33")
  (set-face-foreground 'font-lock-keyword-face       "white")
  (set-face-foreground 'font-lock-string-face        "gray66")
  (set-face-foreground 'font-lock-function-name-face "white")
  (set-face-foreground 'font-lock-variable-name-face "white")
  (set-face-foreground 'font-lock-type-face          "white")
  (set-face-foreground 'font-lock-constant-face      "white")
  (set-face-foreground 'font-lock-builtin-face      "gray44")

;; (set-default-font "-*-6x10-normal-r-*-*-11-82-*-*-c-*-*-#33-")

;;  (set-default-font "-*-ProFontWindows-normal-r-*-*-11-82-96-96-c-*-iso8859-1")

;;  (set-face-font 'font-lock-function-name-face
;;               ;; "-*-clb6x12-normal-i-*-*-12-90-96-96-c-*-*-#33"
;;               ;;"-*-ProFontWindows-bold-r-*-*-11-90-96-96-c-*-iso8859-1"
;;               )
 ;; (set-face-font 'font-lock-variable-name-face
;;               ;; "-*-clb6x12-normal-i-*-*-12-90-96-96-c-*-*-#33"
;;               "-*-ProFontWindows-bold-r-*-*-11-82-96-96-c-*-iso8859-1"
;;               )
  ;;(set-face-font 'default
  ;; "-*-clr6x10-normal-r-*-*-11-82-96-96-c-*-*-#33")

  ;; (w32-select-font)

  (set-cursor-color "white")
  )

;; (ala-charcoal)

(set-face-bold-p 'font-lock-function-name-face t)

(defun ala-dark-default()
  (interactive)
  (ala-background-black)
  (set-face-background 'highlight "darkslateblue")
  (set-face-foreground 'highlight "white")
  (set-face-foreground 'region "black")
  (set-face-background 'region "wheat")

  (set-face-foreground 'font-lock-comment-face "OrangeRed")
  (set-face-foreground 'font-lock-doc-face "OrangeRed")
  (set-face-foreground 'font-lock-keyword-face "LightSteelBlue")
  (set-face-foreground 'font-lock-string-face "LightSalmon")
  (set-face-foreground 'font-lock-function-name-face "LightSkyBlue")
  (set-face-foreground 'font-lock-variable-name-face "LightGoldenrod")
  (set-face-foreground 'font-lock-type-face "PaleGreen")
  (set-face-foreground 'font-lock-constant-face "Aquamarine")
  (set-cursor-color "green")
  )

;; (ala-dark-default)

(defun ala-light-default()
  (interactive)
  (ala-background-white)

  (set-face-foreground 'font-lock-comment-face "Firebrick")
  (set-face-foreground 'font-lock-doc-face "Firebrick")
  (set-face-foreground 'font-lock-keyword-face "Purple")
  (set-face-foreground 'font-lock-string-face "sienna")
  (set-face-foreground 'font-lock-function-name-face "Blue")
  (set-face-foreground 'font-lock-variable-name-face "DarkGoldenrod")
  (set-face-foreground 'font-lock-type-face "DarkOliveGreen")
  ;(set-face-foreground 'font-lock-reference-face "red")
  )

;; (ala-colors-sky)
(defun ala-colors-sky ()
  (interactive)
  (ala-dark-default)
  (set-background-color "skyblue4")
  (set-foreground-color "black")
  (set-face-foreground 'font-lock-comment-face "Gray60")
  (set-face-foreground 'font-lock-string-face "midnight blue")
  )

(defun ala-colors-1 ()
  (interactive)
  (ala-background-black)

  (set-face-foreground 'font-lock-comment-face "darkslateblue")
  (set-face-foreground 'font-lock-doc-face "darkslateblue")
  (set-face-foreground 'font-lock-keyword-face "Coral")
  (set-face-foreground 'font-lock-function-name-face "Green")
  (set-face-foreground 'font-lock-variable-name-face "Gold")
  (set-face-foreground 'font-lock-type-face "Coral")
  ;(set-face-foreground 'font-lock-reference-face "yellow")
  )

;; (defun ala-colors-2 ()
;;   (interactive)
;;   (ala-background-white)

;;   (set-face-foreground 'font-lock-comment-face "DarkGreen")
;;   (set-face-foreground 'font-lock-doc-face "DarkGreen")
;;   (set-face-foreground 'font-lock-string-face "DarkBlue")
;;   (set-face-foreground 'font-lock-keyword-face "RoyalBlue")
;;   (set-face-foreground 'font-lock-function-name-face "Blue")
;;   (set-face-foreground 'font-lock-variable-name-face "Black")
;;   (set-face-foreground 'font-lock-type-face "Black")
;;   ;;(set-face-foreground 'font-lock-reference-face "Purple")
;;   )

;; (ala-colors-2)
;; (ala-background-color "gray50")
;; (ala-background-color "white")

(defun ala-colors-3 ()
  (interactive)
  (ala-light-default)

  (ala-background-color "wheat")
  (set-face-background 'region "Darkslateblue")
  (set-face-foreground 'region "Gray90")
  (set-face-foreground 'font-lock-string-face "gray30")
  (set-face-foreground 'font-lock-comment-face "Firebrick4")
  (set-face-foreground 'font-lock-doc-face "Firebrick4")
  (set-face-foreground 'font-lock-warning-face "red")
  (set-face-foreground 'font-lock-keyword-face "darkorchid4")
  (set-face-foreground 'font-lock-type-face "purple4")
  (set-face-foreground 'font-lock-builtin-face "mediumpurple4")
  (set-face-foreground 'font-lock-constant-face "Blue4")
  (set-face-foreground 'font-lock-variable-name-face "DarkGreen")
  (set-face-foreground 'font-lock-function-name-face "medium blue")
  (set-face-bold-p 'font-lock-type-face t)
  (set-face-bold-p 'font-lock-function-name-face t)
  (set-cursor-color "red")
)

 (defun ala-colors-4 ()
   (interactive)
   (ala-light-default)

  (set-face-background 'default "darkslateblue")
  (set-face-foreground 'default "white")
  (set-background-color "darkslateblue")
  (set-foreground-color "white")

  (set-face-foreground 'region "black")
  (set-face-background 'region "wheat")

;  (set-face-foreground 'modeline "black")
;  (set-face-background 'modeline "wheat")

  (set-face-foreground 'font-lock-string-face "gold")
  (set-face-foreground 'font-lock-function-name-face "deepskyblue1")
  (set-face-foreground 'font-lock-keyword-face "LightSteelBlue")
  (set-face-foreground 'font-lock-comment-face "tan1")
  (set-face-foreground 'font-lock-doc-face "tan1")
  (set-face-foreground 'font-lock-variable-name-face "wheat")
  (set-face-foreground 'font-lock-constant-face "tomato1")
  (set-face-foreground 'font-lock-warning-face "red")
  (set-face-foreground 'font-lock-type-face "PaleGreen")
  )

;; (ala-colors-4)

(defun ala-colors-5 ()
  (interactive)
  (ala-colors-4)
  (set-face-foreground 'font-lock-constant-face "skyblue2")
  (ala-background-color "darkslategray"))

;; (ala-colors-5)

(defun ala-colors-6 ()
  (interactive)
  (ala-dark-default)
  ;; (ala-colors-5)

  (set-face-foreground 'font-lock-comment-face "orange red")
  (set-face-foreground 'font-lock-doc-face "orange red")

  (ala-background-color "gray15")
  ;; (set-face-foreground 'default "blanched almond")
  (set-face-foreground 'default "white")
  ;; (set-face-foreground 'default "papaya whip")

  (set-face-background 'default "gray15"))


(defun ala-set-primary-colors (fg bg)
  (set-face-foreground 'default fg)
  (set-face-background 'default bg)
  ;; (set-face-background 'fringe bg)
;  (set-face-foreground 'modeline bg)
;  (set-face-background 'modeline fg)
  (set-foreground-color fg)
  (set-background-color bg))

(defun ala-colors-7 ()
  (interactive)
  (ala-set-primary-colors "gray95" "gray20")
;  (set-face-foreground 'modeline "black")

  (set-face-background 'highlight "darkslateblue")
  (set-face-foreground 'highlight "gray95")
  (set-face-foreground 'region "darkslateblue")
  (set-face-background 'region "gray95")

  (set-face-foreground 'font-lock-comment-face "tan")
  (set-face-foreground 'font-lock-doc-face "tan")
  (set-face-foreground 'font-lock-keyword-face "DarkGoldenrod")
  (set-face-foreground 'font-lock-variable-name-face "Gold")
  ;(set-face-foreground 'font-lock-reference-face "yellow")
  (set-face-foreground 'font-lock-string-face "PaleGreen")
  ;;(set-face-foreground 'font-lock-function-name-face "LightSkyBlue")
  ;;(set-face-foreground 'font-lock-variable-name-face "LightGoldenrod")
  ;; (set-face-foreground 'font-lock-type-face "chartreuse2")
  ;;(set-face-foreground 'font-lock-reference-face "Aquamarine")
  )

(defun ala-colors-green ()
  (interactive)
  (ala-dark-default)
  (ala-set-primary-colors "limegreen" "black")
  (set-face-background 'highlight "green")
  (set-face-foreground 'highlight "black")
  (set-face-foreground 'region "black")
  (set-face-background 'region "limegreen")
  (set-face-foreground 'font-lock-comment-face       "darkolivegreen")
  (set-face-foreground 'font-lock-doc-face       "darkolivegreen")
  (set-face-foreground 'font-lock-string-face        "PaleGreen")
  (set-face-foreground 'font-lock-keyword-face       "yellow")
  (set-face-foreground 'font-lock-function-name-face "springgreen")
  (set-face-foreground 'font-lock-variable-name-face "green")
  (set-face-foreground 'font-lock-constant-face      "palegreen")
  (set-face-bold-p 'font-lock-variable-name-face t)
  (set-cursor-color "red")
  )

;; (ala-colors-green)

(defun ala-colors-red ()
  (interactive)
  (ala-dark-default)
  (ala-set-primary-colors "white" "#550000")
  (set-face-background 'highlight "darkred")
  (set-face-foreground 'highlight "black")
  (set-face-foreground 'region "white")
  (set-face-background 'region "darkred")
  (set-face-foreground 'font-lock-comment-face       "red1")
  (set-face-foreground 'font-lock-doc-face       "red1")
  (set-face-foreground 'font-lock-string-face        "tomato")
  (set-face-foreground 'font-lock-keyword-face       "orange red")
  (set-face-foreground 'font-lock-function-name-face "orange")
  (set-face-foreground 'font-lock-variable-name-face "yellow")
  ;; (set-face-foreground 'font-lock-reference-face "yellow")
  (set-face-foreground 'font-lock-constant-face      "yellow")
  (set-face-foreground 'font-lock-type-face "yellow")
  (set-face-bold-p 'font-lock-variable-name-face t)
  (set-cursor-color "white")
  )

;; (ala-colors-red)

(defun ala-colors-blue ()
  (interactive)
  (ala-dark-default)
  (ala-set-primary-colors "lightsteelblue" "#000022")
  (set-face-background 'highlight "royalblue")
  (set-face-foreground 'highlight "white")
  (set-face-foreground 'region "white")
  (set-face-background 'region "royalblue")
  (set-face-foreground 'font-lock-comment-face       "#6666CC")
  (set-face-foreground 'font-lock-doc-face       "#6666CC")
  (set-face-foreground 'font-lock-string-face        "cornflowerblue")
  ;; (set-face-foreground 'font-lock-comment-face       "darkslateblue")
  ;; (set-face-foreground 'font-lock-doc-face       "darkslateblue")
  (set-face-foreground 'font-lock-keyword-face       "dodger blue")
  (set-face-foreground 'font-lock-function-name-face "dark turquoise")
  (set-face-foreground 'font-lock-variable-name-face "deep sky blue")
  (set-face-foreground 'font-lock-constant-face      "aquamarine")
  (set-face-foreground 'font-lock-type-face "cyan")
  (set-face-bold-p 'font-lock-variable-name-face t)
  (set-cursor-color "white")
  )

;; (ala-colors-blue)

(defun ala-colors-cyan ()
  (interactive)
  (ala-dark-default)
  (ala-set-primary-colors "skyblue1" "black")
  (set-face-background 'highlight "cyan")
  (set-face-foreground 'highlight "black")
  (set-face-foreground 'region "black")
  (set-face-background 'region "limegreen")
  (set-face-foreground 'font-lock-comment-face       "darkolivegreen")
  (set-face-foreground 'font-lock-doc-face       "darkolivegreen")
  (set-face-foreground 'font-lock-string-face        "PaleGreen")
  (set-face-foreground 'font-lock-keyword-face       "yellow")
  (set-face-foreground 'font-lock-function-name-face "springgreen")
  (set-face-foreground 'font-lock-variable-name-face "green")
  (set-face-foreground 'font-lock-constant-face      "palegreen")
  (set-face-bold-p 'font-lock-variable-name-face t)
  (set-cursor-color "red")
  )

;; (ala-colors-cyan)
