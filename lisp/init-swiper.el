(use-package counsel
  :ensure t
  )

(use-package swiper
  :ensure try
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (bind-key "C-s" 'swiper)
    (bind-key "C-c C-r" 'ivy-resume)
    (bind-key "M-x" 'counsel-M-x)
    (bind-key "C-x C-f" 'counsel-find-file)
    (bind-key "<f1> f" 'counsel-describe-function)
    (bind-key "<f1> v" 'counsel-describe-variable)
    (bind-key "<f1> l" 'counsel-load-library)
    (bind-key "<f2> i" 'counsel-info-lookup-symbol)
    (bind-key "<f2> u" 'counsel-unicode-char)
    (bind-key "C-c g" 'counsel-git)
    (bind-key "C-c j" 'counsel-git-grep)
    (bind-key "C-c k" 'counsel-ag)
    (bind-key "C-x l" 'counsel-locate)
    (bind-key "C-S-o" 'counsel-rhythmbox)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))



(provide 'init-swiper)
