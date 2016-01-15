(require 'package)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(labels ((installed-p (packages)
                      (loop for p in packages
                            when (not (package-installed-p p))
                            do (return nil)
                            finally (return t))))
  (let ((packages '(
                    powerline           ; just eye candy
                    git-gutter+         ; show git status
                    rainbow-mode        ; render color strings
                    markdown-mode
                    jinja2-mode
                    lua-mode
                    go-mode
                    nginx-mode
                    ido-vertical-mode   ; better ido completion
                    json-mode           ; stricter JSON mode
                    js3-mode
                    dash-at-point
                    jedi                ; autocomplete for python
                    handlebars-mode
                    web-mode
                    yaml-mode
                    auto-complete
                    dockerfile-mode
                    typescript
                    )))
    (when (not (installed-p packages))
      (package-refresh-contents)
      (dolist (p packages)
        (unless (package-installed-p p)
          (package-install p))))))

(provide 'init-packages)
