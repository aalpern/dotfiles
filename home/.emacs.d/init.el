;; -*- mode: emacs-lisp -*-

(package-initialize)

(require 'cl)

(add-to-list 'load-path "~/.emacs.d/init")
(add-to-list 'load-path "~/.emacs.d/site-lisp")

(require 'init-packages)
(require 'init-options)
(require 'init-themes)
(require 'init-term)
(require 'init-code)
(require 'init-c)
(require 'init-go)
(require 'init-javascript)
(require 'init-java)
;; (require 'init-python)
(require 'init-protobuf)


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
(autoload 'dylan-mode "dylan-mode" "Major mode for editing Dylan(tm) source code.")
(autoload 'clojure-mode "clojure-mode" nil t)
(autoload 'graphviz-dot-mode "graphviz-dot-mode" nil t)

(require 'aalpern-utils)

(set-default-font "Hack 14")

(toggle-frame-maximized)
(aa-load-theme 'aalpern-dark2)
;; (aa-load-theme 'spacedust)
;; (aa-load-theme 'better-github)

(require 'server)
(unless (server-running-p)
  (server-start))
