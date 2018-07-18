;;
;;; Terminal configuration
;;

(require 'xterm-color)
(add-hook 'shell-mode-hook
          (lambda () (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

(provide 'init-term)
