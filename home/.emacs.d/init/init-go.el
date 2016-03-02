(defun aa-go-mode-hook ()
  (remove-hook 'font-lock-mode-hook 'hc-highlight-tabs)
  ;; (hc-toggle-highlight-tabs)
  (setq indent-tabs-mode t))

(add-hook 'go-mode-hook 'aa-go-mode-hook)
(add-hook 'before-save-hook 'gofmt-before-save)

(provide 'init-go)
