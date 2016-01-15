;; Python-specific autocompletion
;;   The python environment accessible to emacs must also have jedi &
;;   epc modules installed.
(require 'jedi)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

(provide 'init-python)
