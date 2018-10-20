(require 'helm-dash)
(setq helm-dash-browser-func 'eww)
(setq helm-dash-docsets-path "~/.docset")
(setq helm-dash-common-docsets '("C" "C++" "Qt_5" "Emacs_Lisp"))


(if (>= emacs-major-version 26)
    (progn
      (require 'helpful)
      (define-key global-map (kbd "<f1> v") 'helpful-variable)
      (define-key global-map (kbd "<f1> f") 'helpful-function)
      (define-key global-map (kbd "<f1> k") 'helpful-key)
      (define-key global-map (kbd "<f1> m") 'describe-mode)
      (define-key global-map (kbd "<f1> i") 'info))
  (progn
    (define-key global-map (kbd "<f1> v") 'describe-variable)
    (define-key global-map (kbd "<f1> f") 'describe-function)
    (define-key global-map (kbd "<f1> k") 'describe-key)
    (define-key global-map (kbd "<f1> m") 'describe-mode)
    (define-key global-map (kbd "<f1> i") 'info)))


(define-key global-map (kbd "<f1> d") 'find-function)
(define-key global-map (kbd "<f1> l") 'find-library)


(provide 'init-document)
