;; -*- mode: emacs-lisp -*-

(require 'cl)

;; ----------------------------------------------------------------------
;;; Load Path
;; ----------------------------------------------------------------------

(add-to-list 'load-path              "~/.emacs.d/site-lisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; ----------------------------------------------------------------------
;;; Packages
;; ----------------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(labels ((installed-p (packages)
                      (loop for p in packages
                            when (not (package-installed-p p))
                            do (return nil)
                            finally (return t))))
  (let ((packages '(
                    powerline
                    git-gutter+
                    protobuf-mode
                    rainbow-mode
                    markdown-mode
                    jinja2-mode
                    lua-mode
                    ido-vertical-mode
                    )))
    (when (not (installed-p packages))
      (package-refresh-contents)
      (dolist (p packages)
        (unless (package-installed-p p)
          (package-install p))))))

;; ----------------------------------------------------------------------
;;; Miscellany
;; ----------------------------------------------------------------------

;; Get rid of UI bloat
(blink-cursor-mode 0)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(scroll-bar-mode 0)
(tool-bar-mode 0)

;; Set up the mode line
(column-number-mode t)
(display-time-mode t)
(line-number-mode t)

;; Set OS X key bindings
(setq x-super-keysym 'meta)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier  'super)

;; Basic editing settings
(setq truncate-partial-width-windows nil)
(put 'upcase-region   'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(setq require-final-newline t)
(setq-default suggest-key-bindings t)
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(setq indent-tabs-mode nil)
(show-paren-mode 1)

;; Modern-looking modeline
(require 'powerline)
(powerline-default-theme)

;; Git integration
(require 'git-gutter+)
;; Uncomment these lines for the fringe version, which can be used
;; with linenum mode or put on the right side of the buffer.
;;(require 'fringe-helper)
;;(require 'git-gutter-fringe+)
(global-git-gutter+-mode t)

;; Better buffer switching
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)

;; ----------------------------------------------------------------------
;;; Extended Configuration
;;    Load separate files for C/C++, Java, and highlighting support
;; ----------------------------------------------------------------------

(load "~/.emacs.d/init-c.el")
(load "~/.emacs.d/init-java.el")
(load "~/.emacs.d/init-color.el")

(require 'show-wspace)
(toggle-show-tabs-show-ws)
(show-ws-toggle-show-trailing-whitespace)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ----------------------------------------------------------------------
;;; Auto Mode List
;; ----------------------------------------------------------------------

(setq auto-mode-alist
      '(
        ;; SQL
        ("\\.\\(sql\\|prc\\|TRG\\|TAB\\)$"                               . sql-mode)
        ("\\.\\(dir\\|dat\\)$"                                           . hexl-mode)
        ;; Assorted and Sundry C varieties
        ("\\.c$"                                                         . c++-mode)
        ("\\.h$"                                                         . c++-mode)
        ("\\.y$"                                                         . c-mode)
        ("\\.l$"                                                         . c-mode)
        ("\\.php$"                                                       . c++-mode)
        ("\\.m$"                                                         . objc-mode)
        ("\\.cp$"                                                        . c++-mode)
        ("\\.rb$"                                                        . ruby-mode)
        ("\\.[CH]$"                                                      . c++-mode)
        ("\\.proto"                                                      . protobuf-mode)
        ("\\.json"                                                       . javascript-mode)
        ("\\.rst"                                                        . rst-mode)
        ("\\.\\(cc\\|hh\\|cs\\|pde\\)$"                                  . c++-mode)
        ("\\.\\(tli\\|tlh\\)$"                                           . c++-mode)
        ("\\.\\(cxx\\|hxx\\)$"                                           . c++-mode)
        ("\\.\\(cpp\\|hpp\\|inl\\)$"                                     . c++-mode)
        ("\\.\\(java\\|mocha\\|policy\\|jsp\\|jad\\|j\\|bsh\\|djava\\)$" . java-mode)
        ("\\.\\(mm\\|m\\)$"                                              . objc-mode)
        ("\\.\\(idl\\|midl\\)$"                                          . c++-mode) ;; idl-mode is probably good enough to use now, look into it
        ("\\.rb$"                                                        . ruby-mode)
        ("\\.proto$"                                                     . java-mode)
        ;; Assorted and sundry LISP varieties
        ("\\.scm$"                                                       . scheme-mode)
        ("\\.el$"                                                        . emacs-lisp-mode)
        ("\\.emacs*"                                                     . emacs-lisp-mode)
        ("\\.clj$"                                                       . lisp-mode)
        ("\\.lisp$"                                                      . lisp-mode)
        ("\\.lsp$"                                                       . lisp-mode)
        ("\\.system$"                                                    . lisp-mode) ; defsystem files
        ("\\.\\(dyl\\|dylan\\)"                                          . dylan-mode)
        ("\\.s$"                                                         . asm-mode)
        ("\\.tar$"                                                       . tar-mode)
        ("\\.[A-Ya-y]*shrc$"                                             . shell-script-mode)
        ("\\.dbx*"                                                       . shell-script-mode)
        ("\\.\\(texi\\|txi\\)$"                                          . texinfo-mode)
        ("\\.\\(md\\|markdown\\)$"                                       . markdown-mode)
        ("\\.tex$"                                                       . latex-mode)
        ("\\.bib$"                                                       . bibtex-mode)
        ;; Makefiles
        ("[mM]akefile$"                                                  . makefile-mode)
        ("[mM]akefile.*$"                                                . makefile-mode)
        ("Imakefile$"                                                    . makefile-mode)
        ("\\.\\(mak\\|properties\\|mk\\)$"                               . makefile-mode)
        ;; Miscellaneous & sundry scripting languages
        ("\\.\\(py\\|python\\)$"                                         . python-mode)
        ("\\.lua$"                                                       . lua-mode)
        ("\\.\\(pl\\|pm\\)$"                                             . perl-mode)
        ("\\.php$"                                                       . php-mode)
        ("\\.js$"                                                        . javascript-mode)
        ("\\.g"                                                          . antlr-mode)
        ;; Markup
        ("\\.\\(sgml\\|dtd\\)"                                           . sgml-mode)
        ("\\.\\(xml\\|cml\\|xsd\\|idx\\|iqx\\)"                          . xml-mode)
        ("\\.\\(aspx\\|asp\\|resx\\|ascx\\|msbuild\\)$"                  . xml-mode)
        ("\\.\\(htm\\|html\\|htmx\\)$"                                   . html-mode)
        ("\\.css$"                                                       . css-mode)
        ;; Windows stuff
        ("\\.bat$"                                                       . bat-generic-mode)
        ("\\.cmd$"                                                       . bat-generic-mode)
        ("\\.\\(ini\\|cfg\\)$"                                           . ini-generic-mode)
        ("\\.inf$"                                                       . inf-generic-mode)
        ("\\.reg$"                                                       . reg-generic-mode)
        ("\\.rc$"                                                        . rc-generic-mode)
        ("\\.rc2$"                                                       . rc-generic-mode)
        ("\\.rul$"                                                       . rul-generic-mode)
        ("\\.isl$"                                                       . isl-mode)
        ;; Graphviz Dot
        ("\\.dot$"                                                       . graphviz-dot-mode)
        ("\\.txt$"                                                       . text-mode)
        ("$"                                                             . fundamental-mode)
        ))

;; ----------------------------------------------------------------------
;;; Autoload
;; ----------------------------------------------------------------------

(autoload 'jinja2-mode "jinja2-mode" "Jinja2 editing mode." t)
(autoload 'markdown-mode "markdown-mode" "Markdown editing mode." t)
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
(autoload 'javascript-mode "javascript" nil t)
(autoload 'css-mode "css-mode" nil t)
(autoload 'bat-generic-mode "generic-extras" "Mode for editing DOS batch files" t)
(autoload 'ini-generic-mode "generic-extras" "Mode for editing DOS config files" t)
(autoload 'inf-generic-mode "generic-extras" "Mode for editing Windows INF files" t)
(autoload 'reg-generic-mode "generic-extras" "Mode for editing Windows Registry files" t)
(autoload 'rc-generic-mode  "generic-extras" "Mode for editing Windows Resource files" t)
(autoload 'rul-generic-mode "generic-extras" "Mode for editing InstallShield RUL files" t)
(autoload 'dylan-mode "dylan-mode" "Major mode for editing Dylan(tm) source code.")
(autoload 'clojure-mode "clojure-mode" nil t)
(autoload 'graphviz-dot-mode "graphviz-dot-mode" nil t)
(autoload 'protobuf-mode "protobuf-mode" nil t)

;; awesome extension to outline-minor-mode. code which follows the
;; commenting conventions in lisp-mnt.el in the emacs distribution
;; really wins big here.
;(when (gnu-emacs-p)
;  (require 'out-xtra))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; compilation

(require 'compile)

(setq compile-command "mvn compile ")

(setq compilation-error-regexp-alist
  (append (list
     ;; works for jikes
     '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):\\([0-9]+\\):[0-9]+:[0-9]+:" 1 2 3)
     ;; works for javac
     '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):" 1 2))
  compilation-error-regexp-alist))

(defconst my-protobuf-style
  '((c-basic-offset . 4)
    (indent-tabs-mode . nil)))

(add-hook 'protobuf-mode-hook
          (lambda () (c-add-style "my-style" my-protobuf-style t)))

(load "~/.emacs.d/aalpern-utils.el")

;;(set-default-font "DejaVu Sans Mono 11")
;;(set-default-font "DejaVu Sans Mono 9")
;;(set-default-font "Inconsolata Medium 10")
;;(set-default-font "6x10")
;;(set-default-font "6x12")
;;(set-default-font "6x13")
;;(set-default-font "Anonymous Pro 8")
;;(set-default-font "Anonymous Pro Bold 10")
;;(set-default-font "Anonymous Pro Bold 14")
;;(set-default-font "Anonymous Pro 12")
;;(set-default-font "Meslo LG L DZ 10")
;;(set-default-font "Meslo LG M DZ 10")
;;(set-default-font "Meslo LG M DZ 12")
;;(set-default-font "Meslo LG M DZ Bold 11")
;;(set-default-font "Meslo LG L DZ 11")
;;(set-default-font "Meslo LG M DZ 11")
;;(set-default-font "Meslo LG S DZ 11")
;;(set-default-font "Meslo LG M DZ 10")
;;(set-default-font "Meslo LG S DZ 10")
;;(set-default-font "ProFont 11")
(set-default-font "Anonymous Pro Bold 14")
;;(set-default-font "Meslo LG S DZ Bold 12")
;;(set-default-font "ProFont 11")
;;(set-default-font "ProFont 12")

(load-theme 'aalpern-dark t)
(server-start)
