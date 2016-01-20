(defun aa-go-mode-hook ()
  (setq indent-tabs-mode t))

(add-hook 'go-mode-hook 'aa-go-mode-hook)
(add-hook 'before-save-hook 'gofmt-before-save)

(provide 'init-go)
