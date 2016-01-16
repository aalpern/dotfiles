;; -*- mode: emacs-lisp -*-

(require 'cl)

;; ----------------------------------------------------------------------
;;; Load Path
;; ----------------------------------------------------------------------

(add-to-list 'load-path              "~/.emacs.d/init")
(add-to-list 'load-path              "~/.emacs.d/site-lisp")

;; ----------------------------------------------------------------------
;;; Packages
;; ----------------------------------------------------------------------

(require 'init-packages)
(require 'init-options)
(require 'init-themes)

;; -----------------------------------------------------------------------------
;; Coding
;; -----------------------------------------------------------------------------


(custom-set-variables
 '(js3-boring-indentation nil)
 '(js3-consistent-level-indent-inner-bracket t)
 '(js3-indent-dots nil)
 '(js-indent-level 2)
 '(typescript-auto-indent-flag t)
 '(typescript-indent-level 2))

;; ----------------------------------------------------------------------
;;; Extended Configuration
;;    Load separate files for C/C++, Java, and highlighting support
;; ----------------------------------------------------------------------

(require 'init-c)
(require 'init-go)
;; (require 'init-java)
;; (require 'init-python)

;; -----------------------------------------------------------------------------
;; Highlighting
;; -----------------------------------------------------------------------------

(setq-default font-lock-auto-fontify  t
              font-lock-use-fonts     t
              font-lock-use-colors    t
              font-lock-use-maximal-decoration  t
              font-lock-mode-disable-list       nil)
(require 'font-lock)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size nil)

;; ----------------------------------------------------------------------
;;; Auto Mode List
;; ----------------------------------------------------------------------

;; SQL
(add-to-list 'auto-mode-alist '("\\.\\(sql\\|prc\\|TRG\\|TAB\\)"                               . sql-mode) t)
(add-to-list 'auto-mode-alist '("\\.\\(dir\\|dat\\)"                                           . hexl-mode) t)
;; Assorted and Sundry C varieties
(add-to-list 'auto-mode-alist '("\\.c"                                                         . c++-mode) t)
(add-to-list 'auto-mode-alist '("\\.h"                                                         . c++-mode) t)
(add-to-list 'auto-mode-alist '("\\.y"                                                         . c-mode) t)
(add-to-list 'auto-mode-alist '("\\.l"                                                         . c-mode) t)
(add-to-list 'auto-mode-alist '("\\.php"                                                       . c++-mode) t)
(add-to-list 'auto-mode-alist '("\\.m"                                                         . objc-mode) t)
(add-to-list 'auto-mode-alist '("\\.cp"                                                        . c++-mode) t)
(add-to-list 'auto-mode-alist '("\\.rb"                                                        . ruby-mode) t)
(add-to-list 'auto-mode-alist '("\\.[CH]"                                                      . c++-mode) t)
(add-to-list 'auto-mode-alist '("\\.proto"                                                     . protobuf-mode) t)
(add-to-list 'auto-mode-alist '("\\.json"                                                      . javascript-mode) t)
(add-to-list 'auto-mode-alist '("\\.rst"                                                       . rst-mode) t)
(add-to-list 'auto-mode-alist '("\\.\\(cc\\|hh\\|cs\\|pde\\)"                                  . c++-mode) t)
(add-to-list 'auto-mode-alist '("\\.\\(tli\\|tlh\\)"                                           . c++-mode) t)
(add-to-list 'auto-mode-alist '("\\.\\(cxx\\|hxx\\)"                                           . c++-mode) t)
(add-to-list 'auto-mode-alist '("\\.\\(cpp\\|hpp\\|inl\\)"                                     . c++-mode) t)
(add-to-list 'auto-mode-alist '("\\.\\(java\\|mocha\\|policy\\|jsp\\|jad\\|j\\|bsh\\|djava\\)" . java-mode) t)
(add-to-list 'auto-mode-alist '("\\.\\(mm\\|m\\)"                                              . objc-mode) t)
(add-to-list 'auto-mode-alist '("\\.\\(idl\\|midl\\)"                                          . c++-mode) t)
(add-to-list 'auto-mode-alist '("\\.rb"                                                        . ruby-mode) t)
(add-to-list 'auto-mode-alist '("\\.proto"                                                     . java-mode) t)
;; Assorted and sundry LISP varieties
(add-to-list 'auto-mode-alist '("\\.scm"                                                       . scheme-mode) t)
(add-to-list 'auto-mode-alist '("\\.el"                                                        . emacs-lisp-mode) t)
(add-to-list 'auto-mode-alist '("\\.emacs*"                                                    . emacs-lisp-mode) t)
(add-to-list 'auto-mode-alist '("\\.clj"                                                       . lisp-mode) t)
(add-to-list 'auto-mode-alist '("\\.lisp"                                                      . lisp-mode) t)
(add-to-list 'auto-mode-alist '("\\.lsp"                                                       . lisp-mode) t)
(add-to-list 'auto-mode-alist '("\\.system"                                                    . lisp-mode) t)
(add-to-list 'auto-mode-alist '("\\.\\(dyl\\|dylan\\)"                                         . dylan-mode) t)
(add-to-list 'auto-mode-alist '("\\.s"                                                         . asm-mode) t)
(add-to-list 'auto-mode-alist '("\\.tar"                                                       . tar-mode) t)
(add-to-list 'auto-mode-alist '("\\.[A-Ya-y]*shrc"                                             . shell-script-mode) t)
(add-to-list 'auto-mode-alist '("\\.dbx*"                                                      . shell-script-mode) t)
(add-to-list 'auto-mode-alist '("\\.\\(texi\\|txi\\)"                                          . texinfo-mode) t)
(add-to-list 'auto-mode-alist '("\\.\\(md\\|markdown\\)"                                       . markdown-mode) t)
(add-to-list 'auto-mode-alist '("\\.tex"                                                       . latex-mode) t)
(add-to-list 'auto-mode-alist '("\\.bib"                                                       . bibtex-mode) t)
;; Makefiles
(add-to-list 'auto-mode-alist '("[mM]akefile"                                                  . makefile-mode) t)
(add-to-list 'auto-mode-alist '("[mM]akefile.*"                                                . makefile-mode) t)
(add-to-list 'auto-mode-alist '("Imakefile"                                                    . makefile-mode) t)
(add-to-list 'auto-mode-alist '("\\.\\(mak\\|properties\\|mk\\)"                               . makefile-mode) t)
;; Miscellaneous & sundry scripting languages
(add-to-list 'auto-mode-alist '("\\.\\(py\\|python\\)"                                         . python-mode) t)
(add-to-list 'auto-mode-alist '("\\.lua"                                                       . lua-mode) t)
(add-to-list 'auto-mode-alist '("\\.\\(pl\\|pm\\)"                                             . perl-mode) t)
(add-to-list 'auto-mode-alist '("\\.php"                                                       . php-mode) t)
(add-to-list 'auto-mode-alist '("\\.js"                                                        . javascript-mode) t)
(add-to-list 'auto-mode-alist '("\\.g"                                                         . antlr-mode) t)
;; Markup
(add-to-list 'auto-mode-alist '("\\.\\(sgml\\|dtd\\)"                                          . sgml-mode) t)
(add-to-list 'auto-mode-alist '("\\.\\(xml\\|cml\\|xsd\\|idx\\|iqx\\)"                         . xml-mode) t)
(add-to-list 'auto-mode-alist '("\\.\\(aspx\\|asp\\|resx\\|ascx\\|msbuild\\)"                  . xml-mode) t)
(add-to-list 'auto-mode-alist '("\\.\\(htm\\|html\\|htmx\\)"                                   . html-mode) t)
(add-to-list 'auto-mode-alist '("\\.css"                                                       . css-mode) t)
;; Windows stuff
(add-to-list 'auto-mode-alist '("\\.bat"                                                       . bat-generic-mode) t)
(add-to-list 'auto-mode-alist '("\\.cmd"                                                       . bat-generic-mode) t)
(add-to-list 'auto-mode-alist '("\\.\\(ini\\|cfg\\)"                                           . ini-generic-mode) t)
(add-to-list 'auto-mode-alist '("\\.inf"                                                       . inf-generic-mode) t)
(add-to-list 'auto-mode-alist '("\\.reg"                                                       . reg-generic-mode) t)
(add-to-list 'auto-mode-alist '("\\.rc"                                                        . rc-generic-mode) t)
(add-to-list 'auto-mode-alist '("\\.rc2"                                                       . rc-generic-mode) t)
(add-to-list 'auto-mode-alist '("\\.rul"                                                       . rul-generic-mode) t)
(add-to-list 'auto-mode-alist '("\\.isl"                                                       . isl-mode) t)
;; Graphviz Dot
(add-to-list 'auto-mode-alist '("\\.dot"                                                       . graphviz-dot-mode) t)
(add-to-list 'auto-mode-alist '("\\.txt"                                                       . text-mode) t)
(add-to-list 'auto-mode-alist '("$"                                                            . fundamental-mode) t)

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

(require 'aalpern-utils)

(case system-type
  ('gnu/linux (set-default-font "Meslo LG S DZ 11"))
  ('darwin (set-default-font "Meslo LG S DZ 12")))

(set-default-font "Consolas for BBEdit 13")

;; (add-to-list 'default-frame-alist '(height . 50))
;; (add-to-list 'default-frame-alist '(width . 120))

(toggle-frame-maximized)

(aa-load-theme 'aalpern-dark2)

(require 'server)
(unless (server-running-p)
  (server-start))
