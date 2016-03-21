;;; Basic Emacs Configuration

;; Minimize accidental editor vaporization
(setq confirm-kill-emacs 'yes-or-no-p)

;;
;;; Get rid of UI bloat
;;

(blink-cursor-mode             0)
(setq inhibit-splash-screen    t)
(setq inhibit-startup-message  t)
(setq initial-scratch-message "")
(scroll-bar-mode               0)
(tool-bar-mode                 0)

;; shut. up.
;; (setq visible-bell 0)

;;
;;; Set up the mode line
;;

(column-number-mode t)
(display-time-mode  t)
(line-number-mode   t)

;; Modern-looking modeline
(require 'powerline)
(powerline-default-theme)

;;
;;; Set key bindings
;;

(case system-type
  ('darwin
   (progn
     (setq x-super-keysym 'meta)
     (setq mac-command-modifier 'meta)
     (setq mac-option-modifier  'super))))

;;
;; Stop toggling overwrite-mode on 'insert', which is much too easy to
;; hit on my keyboard.
;;

(define-key global-map [(insert)] nil)
(define-key global-map [(control insert)] 'overwrite-mode)

;;
;;; Basic editing settings
;;

(setq truncate-partial-width-windows nil)
(put 'upcase-region   'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(setq require-final-newline        t)
(setq-default suggest-key-bindings t)
(setq-default indent-tabs-mode     nil)
(setq default-tab-width            4)
(show-paren-mode                   1)

;;
;;; Better buffer switching
;;

(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)

;;
;;; Completion for everything
;;

(require 'auto-complete)
;; (global-auto-complete-mode t)

;;; dynamic everything completion
;;
;; better dynamic-completion.  use m-/ and m-c-/ to access all
;; this awesomeness this is potentially one of the coolest, most
;; useful features of day-to-day text writing in emacs.  well,
;; that may be an overstatement, but damn is it cool. --gzenie

(require 'new-dabbrev) ; dynamic everthing completion
(setq dabbrev-always-check-other-buffers t)
(setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
(setq dabbrev-case-fold-search t)   ;; case insensitive for searching
(setq dabbrev-case-replace nil)     ;; but case sensitive for actual placement

(setq ansible::vault-password-file "/etc/ansible/.vaultpw")

(provide 'init-options)
