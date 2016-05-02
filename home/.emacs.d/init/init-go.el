(defun aa-go-mode-hook ()
  (remove-hook 'font-lock-mode-hook 'hc-highlight-tabs)
  ;; (hc-toggle-highlight-tabs)
  (setq indent-tabs-mode t))

(add-hook 'go-mode-hook 'aa-go-mode-hook)
(add-hook 'before-save-hook 'gofmt-before-save)

;;(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
;;(require 'golint)

(provide 'init-go)
