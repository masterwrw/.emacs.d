(use-package swiper
  :ensure t
  :bind
  ("C-f"   . 'swiper)
  :config
  (ryo-modal-set-key "ff" 'swiper))

;; 安装了 smex 后，counsel-M-x 才会按照使用频率排序
(use-package smex :ensure t)

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (ryo-modal-set-key "a" 'counsel-M-x)
  (ryo-modal-set-key (kbd "SPC o") 'counsel-find-file)
  (ryo-modal-set-key "bl" 'counsel-ibuffer)
  (ryo-modal-set-key "fa" 'counsel-ag)
    
  (let ((command
	 (cond
          ((executable-find "rg")
           "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
          ((executable-find "ag")
           "ag -i --noheading --nocolor --nofilename --numbers '%s' %s"))))
    (setq counsel-grep-base-command command))
  )

(use-package ivy
  :ensure t
  :config
  (define-key ivy-minibuffer-map (kbd "C-i") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "M-o") 'ivy-occur))


(use-package find-file-in-project
  :ensure t)




(provide 'init-ivy)
