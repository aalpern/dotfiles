(require 'init-c)

(defun my-java-mode-hook ()
  (c-set-style "aalpern")
  (auto-fill-mode))

(add-hook 'java-mode-hook 'my-java-mode-hook)

;;(require 'andersl-java-font-lock)

;; Complicated regexp to match method declarations in interfaces or classes
;; A nasty test case is:
;;    else if(foo instanceof bar) {
;; To avoid matching this as a method named "if" must check that within
;; a parameter list there are an even number of symbols, i.e., one type name
;; paired with one variable name.  The trick there is to use the regexp
;; patterns \< and \> to match beginning and end of words.
(defvar java-function-regexp
  (concat
   "^[ \t]*"                                   ; leading white space
   "\\(public\\|private\\|protected\\|"        ; some of these 8 keywords
   "abstract\\|final\\|static\\|"
   "synchronized\\|native"
   "\\|[ \t\n\r]\\)*"                          ; or whitespace
   "[a-zA-Z0-9_$]+"                            ; return type
   "[ \t\n\r]*[[]?[]]?"                        ; (could be array)
   "[ \t\n\r]+"                                ; whitespace
   "\\([a-zA-Z0-9_$]+\\)"                      ; the name we want!
   "[ \t\n\r]*"                                ; optional whitespace
   "("                                         ; open the param list
   "\\([ \t\n\r]*"                             ; optional whitespace
   "\\<[a-zA-Z0-9_$]+\\>"                      ; typename
   "[ \t\n\r]*[[]?[]]?"                        ; (could be array)
   "[ \t\n\r]+"                                ; whitespace
   "\\<[a-zA-Z0-9_$]+\\>"                      ; variable name
   "[ \t\n\r]*[[]?[]]?"                        ; (could be array)
   "[ \t\n\r]*,?\\)*"                          ; opt whitespace and comma
   "[ \t\n\r]*"                                ; optional whitespace
   ")"                                         ; end the param list
   "[ \t\n\r]*"                                ; whitespace
   ;;   "\\(throws\\([, \t\n\r]\\|[a-zA-Z0-9_$]\\)+\\)?{"
   "\\(throws[^{;]+\\)?"                       ; optional exceptions
   "[;{]"                                      ; ending ';' (interfaces) or '{'
   )
  "Matches method names in java code, select match 2")

(defvar java-class-regexp
  "^[ \t\n\r]*\\(final\\|abstract\\|public\\|[ \t\n\r]\\)*class[ \t\n\r]+\\([a-zA-Z0-9_$]+\\)[^;{]*{"
  "Matches class names in java code, select match 2")

(defvar java-interface-regexp
  "^[ \t\n\r]*\\(abstract\\|public\\|[ \t\n\r]\\)*interface[ \t\n\r]+\\([a-zA-Z0-9_$]+\\)[^;]*;"
  "Matches interface names in java code, select match 2")

(defvar java-imenu-regexp
  (list (list nil java-function-regexp 2)
        (list ".CLASSES." java-class-regexp 2)
        (list ".INTERFACES." java-interface-regexp 2))
  "Imenu expression for Java")

;; install it
(add-hook 'java-mode-hook
          (function (lambda ()
                      (setq imenu-generic-expression java-imenu-regexp))))

(require 'compile)
(setq compile-command "mvn compile ")

(add-hook 'compile-mode-hook
          (function (lambda ()
                      (add-to-list 'compilation-error-regexp-alist 'maven)
                      (add-to-list 'compilation-error-regexp-alist-alist
                                   '(maven "\\[ERROR\\] \\(.+?\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\].*"
                                           1 2 3)))))

(setq compilation-error-regexp-alist
      (append (list
               ;; works for jikes
               '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):\\([0-9]+\\):[0-9]+:[0-9]+:" 1 2 3)
               ;; works for javac
               '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):" 1 2))
              compilation-error-regexp-alist))

(provide 'init-java)
