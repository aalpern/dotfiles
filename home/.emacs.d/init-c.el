;; -*- mode: emacs-lisp -*-

(load "cc-mode")
(setq c-font-lock-keywords c-font-lock-keywords-3)
(setq c++-font-lock-keywords c++-font-lock-keywords-3)

(c-add-style "aalpern"
             '("stroustrup"
               (indent-tabs-mode . nil)
               (c-basic-offset . 4)
               (c-comment-only-line-offset . 0)
               (c-recognize-knr-p . nil)
               (c-old-style-variable-behavior . t)
               (c-echo-semantic-information-p . t)
               (c-offsets-alist .
                                ((statement-block-intro .  +)
                                 (knr-argdecl-intro     .  +)
                                 (substatement-open     .  0)
                                 (label                 .  -)
                                 (statement-cont        .  +)
                                 (case-label            .  2)
                                 (access-label          .  -2)
                                 (brace-list-open       .  0)
                                 (brace-list-close      .  0)
                                 (brace-list-intro      .  4)
                                 (brace-list-entry      .  0)
                                 (innamespace           .  2)
                                 (inline-open           .  0)))
               ))

(defun my-c++-mode-hook ()
  (c-set-style "aalpern")
  (auto-fill-mode))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; (define-key c-mode-base-map (kbd "TAB") 'self-insert-command)
