(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

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
                    go-autocomplete
                    git-gutter-fringe+
                    nginx-mode
                    ido-vertical-mode   ; better ido completion
                    json-mode           ; stricter JSON mode
                    js3-mode
                    dash-at-point
                    string-inflection
                    ;; jedi                ; autocomplete for python
                    handlebars-mode
                    web-mode
                    yaml-mode
                    auto-complete
                    dockerfile-mode
                    typescript
                    ansible
                    ;; highlight-chars
                    toml-mode
                    protobuf-mode
                    xterm-color
                    terraform-mode
                    scss-mode
                    )))
    (when (not (installed-p packages))
      (package-refresh-contents)
      (dolist (p packages)
        (unless (package-installed-p p)
          (package-install p))))))

(provide 'init-packages)
