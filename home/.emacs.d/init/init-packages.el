(require 'package)

;; Workaround for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341,
;; via https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

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
                    cmake-mode
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
                    typescript-mode
                    ansible
                    graphviz-dot-mode
                    twilight-theme
                    ;; highlight-chars
                    toml-mode
                    protobuf-mode
                    xterm-color
                    terraform-mode
                    scss-mode
                    groovy-mode
                    gradle-mode
                    clang-format+
                    )))
    (when (not (installed-p packages))
      (package-refresh-contents)
      (dolist (p packages)
        (unless (package-installed-p p)
          (package-install p))))))

(provide 'init-packages)
