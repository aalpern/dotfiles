;; -*- mode: emacs-lisp -*-

;; In case Emacs was dumped with BOCM
;(fmakunbound 'c-mode)
;(makunbound 'c-mode-map)
;(fmakunbound 'c++-mode)
;(makunbound 'c++-mode-map)
;(makunbound 'c-style-alist)

;; (setq load-path (cons "c:/dev/tools/site-lisp/cs-mode-4" load-path))
;; (setq load-path (cons "c:/dev/gnu/site-lisp/custom/" load-path))
;; (setq load-path (cons "c:/dev/tools/site-lisp/cc-mode-5.25" load-path))
(setq load-path (cons "c:/dev/tools/site-lisp/cc-mode-5.30.9" load-path))

(load "cc-mode")
(autoload 'c++-mode  "cc-mode" "c++ editing mode"         t)
(autoload 'csharp-mode  "cc-mode" "c# editing mode"       t)
(autoload 'c-mode    "cc-mode" "c editing mode"           t)
(autoload 'objc-mode "cc-mode" "objective-c editing mode" t)
(autoload 'protobuf-mode "protobuf-mode" "google protocol buffers editing mode" t)

(defun my-cc-mode-common-hook ()
  '(c-set-offset 'substatement-open 1)
  )

; (define-key c-mode-base-map (kbd "TAB") 'self-insert-command)

(add-hook 'c-mode-common-hook 'my-cc-mode-common-hook)
(add-hook 'c++-mode-common-hook 'my-cc-mode-common-hook)

(setq c-recognize-knr-p nil   ; faster w/o knr support
      c-basic-offset 4        ; the basic multiple for indentation
      c-old-style-variable-behavior t
      c-echo-semantic-information-p t
      c-macro-preprocessor "gcpp")

(if (not (boundp 'c-style-alist))
    (setq c-style-alist nil))

(setq c-style-alist
      (cons '("user"
              (c-basic-offset . 4)
              (c-comment-only-line-offset . 0)
              (c-offsets-alist .
                               ((statement-block-intro .  +)
                                (knr-argdecl-intro     .  +)
                                (substatement-open     .  0)
                                (label                 .  -)
                                (statement-cont        .  +)
                                (case-label            .  2)
                                (access-label          . -2)
                                (brace-list-open       .  0)
                                (brace-list-close      .  0)
                                (brace-list-intro      .  4)
                                (brace-list-entry      .  0)
                                (inline-open           .  0)
                                )))
            c-style-alist))

(add-hook 'c++-mode-hook
      (lambda ()
        (setq indent-tabs-mode t)
        (setq c-basic-offset 4)
        (setq tab-width 4)
        (if (fboundp 'c-set-style)
        (c-set-style "user"))))


(add-hook 'java-mode-hook
      (lambda ()
        (if (fboundp 'c-set-style)
        (c-set-style "user"))))


;; ;; (autoload 'csharp-mode "cc-mode")

;; (c-add-style "myC#Style"
;;   '("C#"
;;   (c-basic-offset . 4)
;;   (c-comment-only-line-offset . (0 . 0))
;;   (c-offsets-alist . (
;;     (c                     . c-lineup-C-comments)
;;     (inclass        . 0)
;;     (namespace-open     . +)
;;     (namespace-close    . +)
;;     (innamespace    . 0)
;;     (class-open         . +)
;;     (class-close    . +)
;;     (inclass        . 0)
;;     (defun-open         . +)
;;     (defun-block-intro     . 0)
;;     (inline-open    . +)
;;     (inline-close       . 0)
;;     (statement-block-intro . 0)
;;     (statement-cont     . +)
;;     (brace-list-intro      . +)
;;     (topmost-intro-cont    . 0)
;;     (block-open         . +)
;;     (block-close    . 0)
;;     (arglist-intro      . +)
;; ;    (arglist-cont      . 0)
;;     (arglist-close      . 0)
;;     ))
;;   ))

;; (defun my-csharp-mode-hook ()
;;   (cond (window-system
;;   (turn-on-font-lock)
;;   (c-set-style "myC#Style")
;;   )))
;; (add-hook 'csharp-mode-hook 'my-csharp-mode-hook)
;; (setq auto-mode-alist
;;       (append '(
;;      ("\\.cs$" . csharp-mode)
;;      ) auto-mode-alist ))

;; ;(setq compilation-error-regexp-alist
;; ;    (append '(
;; ;;C# Compiler
;; ;;t.cs(6,18): error SC1006: Name of constructor must match name of class
;; ;;
;; ;("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)[,]\\([0-9]+\\)): \\(error\\|warning\\) CS[0-9]+:" 1 3 4)
;; ;        )
;; ;    compilation-error-regexp-alist));


(electric-indent-mode +1)
