;;; Newlisp configuration

(require-package 'newlisp-mode)
(require 'newlisp-mode)


(define-key newlisp-mode-map [(control x) (control e)] 'newlisp-eval-last-sexp)

(define-key newlisp-mode-map (kbd "<f5>") 'newlisp-eval-buffer)



(provide 'init-newlisp-env)
