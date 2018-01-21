;;; Racket programming environment

(require-package 'racket-mode)

(add-hook 'racket-mode-hook
          (lambda ()
	    (define-key racket-mode-map (kbd "<f5>") 'racket-send-last-sexp)
            (define-key racket-mode-map (kbd "C-<f5>") 'racket-run)))



(provide 'init-racket-env)
