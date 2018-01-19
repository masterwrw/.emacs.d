;;; Common lisp environment

(require-package 'slime)
(require 'slime)

(setq inferior-lisp-program "/usr/bin/clisp")
(setq slime-contribs '(slime-fancy))



(provide 'init-clisp-env)
