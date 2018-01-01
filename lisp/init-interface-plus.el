;; swiper configuration
;; Using swiper so ido no longer needed
;(require 'init-ido)
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


;; smex configuration
(require-package 'smex)

;; smex or counsel-M-x?
(defvar my-use-smex nil
  "Use `smex' instead of `counsel-M-x' when press M-x.")

(defun my-M-x ()
  (interactive)
  (cond
    (my-use-smex
      (smex))
    ((fboundp 'counsel-M-x)
     ;; `counsel-M-x' will use `smex' to remember history
     (counsel-M-x))
    ((fboundp 'smex)
     (smex))
    (t
      (execute-extended-command))))
(global-set-key (kbd "M-x") 'my-M-x)
(global-set-key (kbd "C-x C-m") 'my-M-x)


;; tabbar
(use-package tabbar
  :ensure t
  :config
  (tabbar-mode 1))




(provide 'init-interface-plus)
