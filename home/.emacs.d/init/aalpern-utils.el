;; =============================================================================
;;; key bindings

;; setup ctrl-c
(defvar ctl-c-keymap nil)
(setq ctl-c-keymap (make-keymap))
(defun ctl-c-prefix () ctl-c-keymap)
(global-set-key "\C-c" (ctl-c-prefix))
;; (global-set-key "\t" (tab-to-tab-stop))

(define-key esc-map "h" 'eval-buffer)
(define-key esc-map "g" 'goto-line)
(define-key esc-map " " 'lisp-complete-symbol)
(define-key esc-map "c" 'compile)
(define-key esc-map "a" 'align-current)

;; =============================================================================
;;; Identifier generation

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
  "Insert a random iOS APNS device token (64 hex characters)."
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
    ("^#.*"      . 'font-lock-comment-face) ; comments at start of line
    ("^<.*?>"    . 'font-lock-function-name-face) ; LHS nonterminals
    ("<.*?>"     . 'font-lock-builtin-face) ; other nonterminals
    ("::="       . 'font-lock-warning-face) ; "goes-to" symbol
    ("\|"        . 'font-lock-warning-face) ; "OR" symbol
    ("\{:\\|:\}" . 'font-lock-keyword-face) ; special pybnf delimiters
    )
  '("\\.bnf\\'" "\\.pybnf\\'") ; filename suffixes
  nil ;; extra function hooks
  "Major mode for BNF highlighting.")

;; =============================================================================
;;; simple comment insertion

(defvar comment-mode-prefix-alist nil)
(setq comment-mode-prefix-alist
      '((emacs-lisp-mode  . ";; ")
        (lisp-mode        . ";; ")
        (c++-mode         . "//")
        (c-mode           . "/* ")
        (javascript-mode  . "/* ")
        (csharp-mode      . "// ")
        (dylan-mode       . "// ")
        (java-mode        . "// ")
        (antlr-mode       . "// ")
        (protobuf-mode    . "// ")
        (go-mode          . "// ")
        (dockerfile-mode  . "# ")
        (makefile-mode    . "# ")
        (perl-mode        . "# ")
        (python-mode      . "# ")
        (bnf-mode         . "# ")
        (sgml-mode        . "<!-- ")
        (html-mode        . "<!-- ")
        (jinja2-mode      . "<!-- ")
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
        (javascript-mode  . " */")
        (dylan-mode       . "")
        (java-mode        . "")
        (makefile-mode    . "")
        (perl-mode        . "")
        (python-mode      . "")
        (sgml-mode        . " -->")
        (html-mode        . " -->")
        (jinja2-mode      . " -->")
        (sh-mode          . "")
        (sql-mode         . "")
        (bat-generic-mode . "")
        (text-mode        . "")))

(defvar *comment-mode-last-char* "=")

(defvar *comment-mode-length* 76)
(setq *comment-mode-length* 77)

(defun comment-mode-prefix ()
  (interactive)
  (or (cdr (assoc major-mode comment-mode-prefix-alist)) ""))

(defun comment-mode-postfix ()
  (interactive)
  (or (cdr (assoc major-mode comment-mode-postfix-alist)) ""))

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

(provide 'aalpern-utils)
