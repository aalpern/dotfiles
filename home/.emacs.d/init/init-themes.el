(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/mine")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/colour-schemes")

(defun aa-load-theme (theme)
  "Load a color theme and force powerline to redraw. It'd be
nice if custom.el had a hook for running after a theme loaded."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes))))))
  (unless (custom-theme-name-valid-p theme)
    (error "Invalid theme name `%s'" theme))
  (load-theme theme t)
  (powerline-reset))

(provide 'init-themes)
