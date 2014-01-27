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
                    powerline           ; just eye candy
                    git-gutter+         ; show git status
                    protobuf-mode
                    rainbow-mode        ; render color strings
                    markdown-mode
                    jinja2-mode
                    lua-mode
                    ido-vertical-mode   ; better ido completion
                    json-mode           ; stricter JSON mode
                    jtags               ; better etags for java
                    jedi                ; autocomplete for python
                    )))
    (when (not (installed-p packages))
      (package-refresh-contents)
      (dolist (p packages)
        (unless (package-installed-p p)
          (package-install p))))))

;; -----------------------------------------------------------------------------
;;; Basic UI and Editing
;; -----------------------------------------------------------------------------

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

;; Better buffer switching
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)

;; -----------------------------------------------------------------------------
;; Coding
;; -----------------------------------------------------------------------------

;; Git integration
(require 'git-gutter+)
;; Uncomment these lines for the fringe version, which can be used
;; with linenum mode or put on the right side of the buffer.
;;(require 'fringe-helper)
;;(require 'git-gutter-fringe+)
(global-git-gutter+-mode t)

;; Helper for lazy fingers
(require 'auto-complete)
(global-auto-complete-mode t)

;; Python-specific autocompletion
;;   The python environment accessible to emacs must also have jedi &
;;   epc modules installed.
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

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

;; SQL
(add-to-list 'auto-mode-alist '("\\.\\(sql\\|prc\\|TRG\\|TAB\\)$"                               . sql-mode))
(add-to-list 'auto-mode-alist '("\\.\\(dir\\|dat\\)$"                                           . hexl-mode))
;; Assorted and Sundry C varieties
(add-to-list 'auto-mode-alist '("\\.c$"                                                         . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h$"                                                         . c++-mode))
(add-to-list 'auto-mode-alist '("\\.y$"                                                         . c-mode))
(add-to-list 'auto-mode-alist '("\\.l$"                                                         . c-mode))
(add-to-list 'auto-mode-alist '("\\.php$"                                                       . c++-mode))
(add-to-list 'auto-mode-alist '("\\.m$"                                                         . objc-mode))
(add-to-list 'auto-mode-alist '("\\.cp$"                                                        . c++-mode))
(add-to-list 'auto-mode-alist '("\\.rb$"                                                        . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.[CH]$"                                                      . c++-mode))
(add-to-list 'auto-mode-alist '("\\.proto"                                                      . protobuf-mode))
(add-to-list 'auto-mode-alist '("\\.json"                                                       . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.rst"                                                        . rst-mode))
(add-to-list 'auto-mode-alist '("\\.\\(cc\\|hh\\|cs\\|pde\\)$"                                  . c++-mode))
(add-to-list 'auto-mode-alist '("\\.\\(tli\\|tlh\\)$"                                           . c++-mode))
(add-to-list 'auto-mode-alist '("\\.\\(cxx\\|hxx\\)$"                                           . c++-mode))
(add-to-list 'auto-mode-alist '("\\.\\(cpp\\|hpp\\|inl\\)$"                                     . c++-mode))
(add-to-list 'auto-mode-alist '("\\.\\(java\\|mocha\\|policy\\|jsp\\|jad\\|j\\|bsh\\|djava\\)$" . java-mode))
(add-to-list 'auto-mode-alist '("\\.\\(mm\\|m\\)$"                                              . objc-mode))
(add-to-list 'auto-mode-alist '("\\.\\(idl\\|midl\\)$"                                          . c++-mode))
(add-to-list 'auto-mode-alist '("\\.rb$"                                                        . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.proto$"                                                     . java-mode))
;; Assorted and sundry LISP varieties
(add-to-list 'auto-mode-alist '("\\.scm$"                                                       . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.el$"                                                        . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.emacs*"                                                     . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.clj$"                                                       . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.lisp$"                                                      . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.lsp$"                                                       . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.system$"                                                    . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.\\(dyl\\|dylan\\)"                                          . dylan-mode))
(add-to-list 'auto-mode-alist '("\\.s$"                                                         . asm-mode))
(add-to-list 'auto-mode-alist '("\\.tar$"                                                       . tar-mode))
(add-to-list 'auto-mode-alist '("\\.[A-Ya-y]*shrc$"                                             . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.dbx*"                                                       . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.\\(texi\\|txi\\)$"                                          . texinfo-mode))
(add-to-list 'auto-mode-alist '("\\.\\(md\\|markdown\\)$"                                       . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.tex$"                                                       . latex-mode))
(add-to-list 'auto-mode-alist '("\\.bib$"                                                       . bibtex-mode))
;; Makefiles
(add-to-list 'auto-mode-alist '("[mM]akefile$"                                                  . makefile-mode))
(add-to-list 'auto-mode-alist '("[mM]akefile.*$"                                                . makefile-mode))
(add-to-list 'auto-mode-alist '("Imakefile$"                                                    . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.\\(mak\\|properties\\|mk\\)$"                               . makefile-mode))
;; Miscellaneous & sundry scripting languages
(add-to-list 'auto-mode-alist '("\\.\\(py\\|python\\)$"                                         . python-mode))
(add-to-list 'auto-mode-alist '("\\.lua$"                                                       . lua-mode))
(add-to-list 'auto-mode-alist '("\\.\\(pl\\|pm\\)$"                                             . perl-mode))
(add-to-list 'auto-mode-alist '("\\.php$"                                                       . php-mode))
(add-to-list 'auto-mode-alist '("\\.js$"                                                        . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.g"                                                          . antlr-mode))
;; Markup
(add-to-list 'auto-mode-alist '("\\.\\(sgml\\|dtd\\)"                                           . sgml-mode))
(add-to-list 'auto-mode-alist '("\\.\\(xml\\|cml\\|xsd\\|idx\\|iqx\\)"                          . xml-mode))
(add-to-list 'auto-mode-alist '("\\.\\(aspx\\|asp\\|resx\\|ascx\\|msbuild\\)$"                  . xml-mode))
(add-to-list 'auto-mode-alist '("\\.\\(htm\\|html\\|htmx\\)$"                                   . html-mode))
(add-to-list 'auto-mode-alist '("\\.css$"                                                       . css-mode))
;; Windows stuff
(add-to-list 'auto-mode-alist '("\\.bat$"                                                       . bat-generic-mode))
(add-to-list 'auto-mode-alist '("\\.cmd$"                                                       . bat-generic-mode))
(add-to-list 'auto-mode-alist '("\\.\\(ini\\|cfg\\)$"                                           . ini-generic-mode))
(add-to-list 'auto-mode-alist '("\\.inf$"                                                       . inf-generic-mode))
(add-to-list 'auto-mode-alist '("\\.reg$"                                                       . reg-generic-mode))
(add-to-list 'auto-mode-alist '("\\.rc$"                                                        . rc-generic-mode))
(add-to-list 'auto-mode-alist '("\\.rc2$"                                                       . rc-generic-mode))
(add-to-list 'auto-mode-alist '("\\.rul$"                                                       . rul-generic-mode))
(add-to-list 'auto-mode-alist '("\\.isl$"                                                       . isl-mode))
;; Graphviz Dot
(add-to-list 'auto-mode-alist '("\\.dot$"                                                       . graphviz-dot-mode))
(add-to-list 'auto-mode-alist '("\\.txt$"                                                       . text-mode))
(add-to-list 'auto-mode-alist '("$"                                                             . fundamental-mode))

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
;;(set-default-font "Anonymous Pro Bold 14")
;;(set-default-font "Meslo LG S DZ Bold 12")
(set-default-font "Meslo LG S DZ 12")
;;(set-default-font "Meslo LG S DZ 14")
;;(set-default-font "ProFont 11")
;;(set-default-font "ProFont 12")

(load-theme 'aalpern-dark t)
(server-start)
