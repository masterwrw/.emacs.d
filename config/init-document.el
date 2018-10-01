(use-package helm-dash
  :ensure t
  :config
  (setq helm-dash-browser-func 'eww)
  (setq helm-dash-docsets-path "~/.docset")
  (setq helm-dash-common-docsets '("C" "C++" "Qt_5" "Emacs_Lisp")))


(if (>= emacs-major-version 26)
    (use-package helpful
      :ensure t
      :config
      (global-set-key (kbd "<f1> v") 'helpful-variable)
      (global-set-key (kbd "<f1> f") 'helpful-function)
      (global-set-key (kbd "<f1> k") 'helpful-key)
      (global-set-key (kbd "<f1> m") 'describe-mode)
      (global-set-key (kbd "<f1> i") 'info))
  (progn
    (global-set-key (kbd "<f1> v") 'describe-variable)
    (global-set-key (kbd "<f1> f") 'describe-function)
    (global-set-key (kbd "<f1> k") 'describe-key)
    (global-set-key (kbd "<f1> m") 'describe-mode)
    (global-set-key (kbd "<f1> i") 'info)))


(provide 'init-document)
