(defun aa-go-mode-hook ()
  (setq indent-tabs-mode nil))

(add-hook 'go-mode-hook 'aa-go-mode-hook)

(provide 'init-go)
