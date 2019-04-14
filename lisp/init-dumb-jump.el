(require 'dumb-jump)
(with-eval-after-load 'counsel-etags
  (progn
    '(advice-add 'counsel-etags-find-tag-at-point :before #'backward-forward-push-mark-wrapper)
    (advice-add 'dumb-jump-go :before #'backward-forward-push-mark-wrapper)
    ))
(backward-forward-mode t)


(provide 'init-dumb-jump)
