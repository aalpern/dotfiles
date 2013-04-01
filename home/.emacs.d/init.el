;; -*- mode: emacs-lisp -*-

(require 'cl)

;; ----------------------------------------------------------------------
;;; Platform Tests
;; ----------------------------------------------------------------------

(defun x-emacs-p ()
  (string-match "XEmacs\\|Lucid" (emacs-version)))

(defun gnu-emacs-p ()
  (string-match "GNU" (emacs-version)))

(defun gnu-emacs-20-p ()
  (string-match "GNU Emacs 20" (emacs-version)))

(defun win32-p ()
  (or (and (boundp 'window-system)
       (eq window-system 'win32))
      (string-match "nt4.0" (emacs-version))
      (string-match "win95" (emacs-version))
      (string-match "win98" (emacs-version))))


;; ----------------------------------------------------------------------
;;; Load Path
;; ----------------------------------------------------------------------

(setq load-path (cons "~/.emacs.d/site-lisp" load-path))

;; ----------------------------------------------------------------------
;;; Miscellany
;; ----------------------------------------------------------------------

(setq x-super-keysym 'meta)
;; (setq mac-command-key-is-meta true)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier  'super)
(setq inhibit-splash-screen t)
(setq truncate-partial-width-windows nil)
(put 'upcase-region   'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(setq require-final-newline t)             ; for safety, require files to end in a newline
(setq-default suggest-key-bindings t)      ; have emacs inform us of key-bindings
(setq line-number-mode      t)             ; always display what line we're on
(setq column-number-mode    t)             ; and what column
(display-time)                             ; and what time it is

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
    ("\\.\\(sql\\|prc\\|TRG\\|TAB\\)$"                . sql-mode)
    ("\\.\\(dir\\|dat\\)$"    . hexl-mode)
    ;; Assorted and Sundry C varieties
    ("\\.c$"                  . c++-mode)
    ("\\.h$"                  . c++-mode)
    ("\\.y$"                  . c-mode)
    ("\\.l$"                  . c-mode)
    ("\\.php$"                . c-mode)
    ("\\.m$"                  . objc-mode)
    ("\\.cp$"                 . c++-mode)
    ("\\.[CH]$"               . c++-mode)
    ("\\.proto"               . protobuf-mode)
    ("\\.json"               . javascript-mode)
    ("\\.rst"               . rst-mode)
    ("\\.\\(cc\\|hh\\|cs\\|pde\\)$" . c++-mode)
    ("\\.\\(tli\\|tlh\\)$"    . c++-mode)
    ("\\.\\(cxx\\|hxx\\)$"    . c++-mode)
    ("\\.\\(cpp\\|hpp\\|inl\\)$"    . c++-mode)
    ("\\.\\(java\\|mocha\\|policy\\|jsp\\|jad\\|j\\|bsh\\|djava\\)$" . java-mode)
    ("\\.\\(mm\\|m\\)$"       . objc-mode)
    ("\\.\\(idl\\|midl\\)$"   . c++-mode) ;; idl-mode is probably good enough to use now, look into it

    ;; Assorted and sundry LISP varieties
    ("\\.scm$"                . scheme-mode)
    ("\\.el$"                 . emacs-lisp-mode)
    ("\\.emacs*"              . emacs-lisp-mode)
    ("\\.clj$"                . lisp-mode)
    ("\\.lisp$"               . lisp-mode)
    ("\\.lsp$"                . lisp-mode)
    ("\\.system$"             . lisp-mode) ; defsystem files
    ("\\.\\(dyl\\|dylan\\)"   . dylan-mode)

    ("\\.s$"                   . asm-mode)
    ("\\.tar$"                 . tar-mode)
    ("\\.[A-Ya-y]*shrc$"       . shell-script-mode)
    ("\\.dbx*"                 . shell-script-mode)
    ("\\.\\(texi\\|txi\\)$"    . texinfo-mode)
    ("\\.\\(md\\|markdown\\)$" . markdown-mode)
    ("\\.tex$"                 . latex-mode)
    ("\\.bib$"                 . bibtex-mode)

    ;; Makefiles
    ("[mM]akefile$"           . makefile-mode)
    ("[mM]akefile.*$"         . makefile-mode)
    ("Imakefile$"             . makefile-mode)
    ("\\.\\(mak\\|properties\\|mk\\)$"                . makefile-mode)

    ;; Miscellaneous & sundry scripting languages
    ("\\.\\(py\\|python\\)$"  . python-mode)
    ("\\.lua$"                . lua-mode)
    ("\\.\\(pl\\|pm\\)$"      . perl-mode)
    ("\\.php$"                . php-mode)
    ("\\.js$"                . javascript-mode)
    ("\\.g"                  . antlr-mode)

    ;; Markup
    ("\\.\\(sgml\\|dtd\\)" . sgml-mode)
    ("\\.\\(xml\\|cml\\|xsd\\|idx\\|iqx\\)" . xml-mode)
    ("\\.\\(aspx\\|asp\\|resx\\|ascx\\|msbuild\\)$"                . xml-mode)
    ("\\.\\(htm\\|html\\|htmx\\)$" . html-mode)
    ("\\.css$"                . css-mode)

    ;; Windows stuff
    ("\\.bat$"                . bat-generic-mode)
    ("\\.cmd$"                . bat-generic-mode)
    ("\\.\\(ini\\|cfg\\)$"    . ini-generic-mode)
    ("\\.inf$"                . inf-generic-mode)
    ("\\.reg$"                . reg-generic-mode)
    ("\\.rc$"                 . rc-generic-mode)
    ("\\.rc2$"                . rc-generic-mode)
    ("\\.rul$"                . rul-generic-mode)
    ("\\.isl$"                . isl-mode)


    ;; Graphviz Dot
    ("\\.dot$"                . graphviz-dot-mode)

    ("\\.txt$"                . text-mode)
    ("$"                      . fundamental-mode)
    ))

;; ANSI/ISO C++ headers + PJP extra headers. Damn things have no
;; extensions!
;; (when (win32-p)
  (setq auto-mode-alist
    (cons '("\\(algorithm\\|bitset\\|complex\\|deque\\|exception\\|fstream\\|functional\\|iomanip\\|ios\\|iosfwd\\|iostream\\|istream\\|iterator\\|limits\\|list\\|locale\\|map\\|memory\\|new\\|numeric\\|ostream\\|queue\\|set\\|sstream\\|stack\\|stdexcept\\|streambuf\\|string\\|strstream\\|typeinfo\\|utility\\|valarray\\|vector\\|xutility\\|xmemory\\|xstring\\|xlocale\\|xlocmon\\|xloctime\\|xlocinfo\\|xiosbase\\|xstddef\\|xiosbase\\|xtree\\|cfloat\\|cctype\\|cassert\\|cerrno\\|ciso646\\|xlocnum\\|cstddef\\)" . c++-mode)
          auto-mode-alist))

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
(autoload 'sqlplus  "sql-mode" "Run an interactive SQL*plus session in a separate buffer." t)
(autoload 'sql-mode "sql-mode" "Major mode for editing SQL*plus batch files." t)
(autoload 'html-helper-mode "html-helper-mode" "Major mode for editing HTML source.")
(autoload 'dylan-mode "dylan-mode" "Major mode for editing Dylan(tm) source code.")
(autoload 'matlab-mode  "matlab" "Major mode for editing MatLab source code." t)
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)
(autoload 'htmlize-buffer "htmlize" nil t)
(autoload 'isl-mode "isl-mode" nil t)
(autoload 'php-mode "php-mode" nil t)
(autoload 'clojure-mode "clojure-mode" nil t)
(autoload 'graphviz-dot-mode "graphviz-dot-mode" nil t)
(autoload 'antlr-mode "antlr-mode" nil t)

;; -----------------------------------------------------------------------------
;; Wiki support
;; -----------------------------------------------------------------------------

(autoload 'wikipedia-mode "wikipedia-mode.el"
  "Major mode for editing documents in Wikipedia markup." t)
(autoload 'longlines-mode "longlines.el"
   "Minor mode for editing long lines." t)

(add-to-list 'auto-mode-alist '("\\.wiki\\'" . wikipedia-mode))

;; ----------------------------------------------------------------------
;;; Mode-specific configuration
;; ----------------------------------------------------------------------

;; (setq interpreter-mode-alist
;;  (cons '("python" . python-mode) interpreter-mode-alist))


(add-hook 'sql-mode-hook
          (function (lambda () (font-lock-mode 1)
                      (abbrev-mode 0))))

(add-hook 'dylan-mode-hook
      (function (lambda ()
              (font-lock-mode 1))))

(add-hook 'sgml-mode-hook
      (function (lambda ()
              (font-lock-mode 1))))

(random t)

(defun insert-random-uuid ()
  "Insert a random universally unique identifier (UUID).

UUID is a 32 digits hexadecimal formatted in certain way with dash.
Example of a UUID: 1df63142-a513-c850-31a3-535fc3520c3d
."
  (interactive)
  (insert
   (format "%04x%04x-%04x-%04x-%04x-%06x%06x"
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 6))
           (random (expt 16 6)) ) ) )

(defun insert-random-device-token ()
  "Insert a random iOS device token (64 hex characters)."
  (interactive)
  (insert
   (upcase
    (format "%08x%08x%08x%08x%08x%08x%08x%08x"
            (random (expt 16 8))
            (random (expt 16 8))
            (random (expt 16 8))
            (random (expt 16 8))
            (random (expt 16 8))
            (random (expt 16 8))
            (random (expt 16 8))
            (random (expt 16 8))))))

(defun insert-random-device-pin ()
  "Insert a random Blackberry device PIN (8 hex chars)"
  (interactive)
  (insert
   (format "%08x" (random (expt 16 8)))))

(define-generic-mode 'bnf-mode
  () ;; comment char: inapplicable because # must be at start of line
  nil ;; keywords
  '(
    ("^#.*" . 'font-lock-comment-face) ;; comments at start of line
    ("^<.*?>" . 'font-lock-function-name-face) ;; LHS nonterminals
    ("<.*?>" . 'font-lock-builtin-face) ;; other nonterminals
    ("::=" . 'font-lock-warning-face) ;; "goes-to" symbol
    ("\|" . 'font-lock-warning-face) ;; "OR" symbol
    ("\{:\\|:\}" . 'font-lock-keyword-face) ;; special pybnf delimiters
    )
  '("\\.bnf\\'" "\\.pybnf\\'") ;; filename suffixes
  nil ;; extra function hooks
  "Major mode for BNF highlighting.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; follow-mode
;;
;; as you move through the lines of an output buffer (such as from
;; `grep' or `occur'), another window highlights the corresponding
;; line of the source buffer. very cool.

;;(require 'fm)
;;(add-hook 'occur-mode-hook 'fm-start)
;;(add-hook 'compilation-mode-hook 'fm-start)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; window system stuff

;(cond ((and (gnu-emacs-p) window-system)
;       (require 'mldrag)         ; modeline dragging
;       (global-set-key [mode-line down-mouse-1] 'mldrag-drag-mode-line)
;       (global-set-key [vertical-line down-mouse-1]
;              'mldrag-drag-vertical-line)
;       (global-set-key [vertical-scroll-bar c-down-mouse-1]
;                       'mldrag-drag-vertical-line)
;;       (load "mouse-select-2") ; cooler selection behavior, but on
;;                 ; win32 it slightly interferes with
;;                 ; bob's hiromi.el. must debug that
;;                 ; someday.
;       ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; bob's way cool buffer list

;; (require 'hiromi)

;;(set-face-foreground 'buffer-group-title1  "gray80")
;;(set-face-background 'buffer-group-title1m "darkred")
;;(set-face-foreground 'buffer-group-title2  "gray60")
;;(set-face-background 'buffer-group-title2m "maroon3")
;;(set-face-foreground 'buffer-group-header-face "darkred")
;;(set-face-background 'buffer-group-header-face "gray30")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; key bindings

;; setup ctrl-c
(defvar ctl-c-keymap nil)
(setq ctl-c-keymap (make-keymap))
(defun ctl-c-prefix () ctl-c-keymap)
(global-set-key "\C-c" (ctl-c-prefix))
; (global-set-key "\t" (tab-to-tab-stop))

(define-key esc-map "h" 'eval-buffer)
(define-key esc-map "g" 'goto-line)
(define-key esc-map " " 'lisp-complete-symbol)
(define-key esc-map "c" 'compile)
(define-key esc-map "a" 'align-current)

(defun pretty-align-all-the-things ()
  (interactive)
  (align-current))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; simple comment insertion

(defvar comment-mode-prefix-alist nil)
(setq comment-mode-prefix-alist
      '((emacs-lisp-mode  . ";; ")
        (lisp-mode        . ";; ")
        (c++-mode         . "//")
        (c-mode           . "/* ")
        (javascript-mode           . "/* ")
        (csharp-mode      . "// ")
        (dylan-mode       . "// ")
        (java-mode        . "// ")
        (antlr-mode       . "// ")
        (protobuf-mode       . "// ")
        (makefile-mode    . "# ")
        (perl-mode        . "# ")
        (python-mode        . "# ")
        (bnf-mode        . "# ")
        (sgml-mode        . "<!-- ")
        (html-mode        . "<!-- ")
        (jinja2-mode        . "<!-- ")
        (sh-mode          . "# ")
        (sql-mode         . "-- ")
        (bat-generic-mode . "rem ")
        (text-mode        . "")))

(defvar comment-mode-postfix-alist nil)
(setq comment-mode-postfix-alist
      '((emacs-lisp-mode  . "")
        (lisp-mode        . "")
        (csharp-mode      . "")
        (c++-mode         . "")
        (c-mode           . " */")
        (javascript-mode           . " */")
        (dylan-mode       . "")
        (java-mode        . "")
        (makefile-mode    . "")
        (perl-mode        . "")
        (python-mode        . "")
        (sgml-mode        . " -->")
        (html-mode        . " -->")
        (jinja2-mode        . " -->")
        (sh-mode          . "")
        (sql-mode         . "")
        (bat-generic-mode . "")
        (text-mode        . "")))

(defvar *comment-mode-last-char* "=")

(defvar *comment-mode-length* 76)
(setq *comment-mode-length* 77)

(defun comment-mode-prefix ()
  (interactive)
  (cdr (assoc major-mode comment-mode-prefix-alist)))

(defun comment-mode-postfix ()
  (interactive)
  (cdr (assoc major-mode comment-mode-postfix-alist)))

(defun comment-insert-line-of-char (char)
  (dotimes (n *comment-mode-length*)
    (insert char)))

(defun comment-insert-comment-line (char)
  (interactive (comment-char-prompt "character" *comment-mode-last-char*))
  (insert (comment-mode-prefix))
  ;;(insert " ")
  (comment-insert-line-of-char char)
  (insert (comment-mode-postfix))
  (setq *comment-mode-last-char* char))

(defun comment-char-prompt (prompt default)
  (list (let* ((prompt (if default
               (format "%s (default %s): " prompt default)
             (concat prompt ": ")))
           (ans (read-string prompt)))
      (if (zerop (length ans)) default ans))))

(define-key ctl-c-keymap "i" 'comment-insert-comment-line)

;; ------------------------------------------------------------


;; Turn on syntax coloring
(cond ((fboundp 'global-font-lock-mode)
       ;; Turn on font-lock in all modes that support it
       (global-font-lock-mode t)
       ;; maximum colors
       (setq font-lock-maximum-decoration t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; IMENU

(setq imenu-sort-function   'imenu--sort-by-name)
;;  imenu-max-items       30)

(add-hook 'emacs-lisp-mode-hook
          (function
           (lambda ()
             (setq imenu-create-index-function
                   (function imenu-example--create-lisp-index))
         (imenu-add-to-menubar "Functions"))))


(add-hook 'lisp-mode-hook
          (function
           (lambda ()
             (setq imenu-create-index-function
                   (function imenu-example--create-lisp-index))
         (imenu-add-to-menubar "Functions"))))

(add-hook 'c-mode-common-hook
      (function
       (lambda ()
         (setq imenu-create-index-function
           (function imenu-default-create-index-function))
         (imenu-add-to-menubar "Functions"))))


(defconst my-protobuf-style
  '((c-basic-offset . 4)
    (indent-tabs-mode . nil)))

(add-hook 'protobuf-mode-hook
          (lambda () (c-add-style "my-style" my-protobuf-style t)))

;;(set-default-font "-misc-fixed-medium-r-normal--10-100-75-*-c-60-*-*")
;;(set-default-font "DejaVu Sans Mono 11")
;;(set-default-font "-misc-fixed-medium-*-*-*-10-*-*-*-*-*-*-*")
;;(set-default-font "6x10")
;;(set-default-font "6x12")
;;(set-default-font "6x13")
;;(set-default-font "Anonymous Pro 8")
;;(set-default-font "Anonymous Pro Bold 10")
(set-default-font "Anonymous Pro Bold 12")

;(set-default-font "Meslo LG L DZ 10")
;(set-default-font "Meslo LG M DZ 10")
;(set-default-font "Meslo LG M DZ 12")
;(set-default-font "Meslo LG M DZ Bold 11")
;(set-default-font "Meslo LG L DZ 11")
;(set-default-font "Meslo LG M DZ 11")
;(set-default-font "Meslo LG S DZ 11")

;(set-default-font "Meslo LG M DZ 10")
;(set-default-font "Meslo LG S DZ 10")
;(set-default-font "Meslo LG M DZ 9")
(set-default-font "Meslo LG S DZ 9")
;(set-default-font "Meslo LG M DZ 8")
;(set-default-font "Meslo LG S DZ 10")

;; (ala-dark-default)
(ala-colors-3)
;;(ala-colors-6)
;;(ala-colors-7)
;;(ala-colors-green)
;;(ala-colors-red)
;;(ala-colors-blue)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-time-mode t)
 '(fringe-mode 0 nil (fringe))
 '(indent-tabs-mode nil)
 '(scroll-bar-mode (quote right))
 '(tool-bar-mode nil))

(setq default-tab-width 4)
(setq indent-tabs-mode nil)

(message "Emacs: Swap space -- The final frontier")
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(server-start)
