(use-package helm-dash
  :ensure t
  :config
  (setq helm-dash-browser-func 'eww)
  (setq helm-dash-docsets-path "~/.docsets")
  (setq helm-dash-common-docsets '("C" "C++" "Qt" "x86_64_asm")))

(use-package zeal-at-point
  :ensure t
  :config
  (setq zeal-at-point-mode-alist '(c++-mode . ("c" "cpp" "qt")))
  (setq zeal-at-point-mode-alist '(c-mode . ("c"))))

(if (>= emacs-major-version 26)
    (use-package helpful :ensure t))



(provide 'init-document)
