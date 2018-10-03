(require 'swiper)
(ryo-modal-set-key "ff" 'swiper)

;; 安装了 smex 后，counsel-M-x 才会按照使用频率排序
(require 'smex)

(require 'counsel)
(global-set-key (kbd "M-x") 'counsel-M-x)
(ryo-modal-set-key "a" 'counsel-M-x)
(ryo-modal-set-key (kbd "C-. v") 'counsel-yank-pop)
(ryo-modal-set-key (kbd "C-. o") 'counsel-find-file)
(ryo-modal-set-key "bl" 'counsel-ibuffer)
(ryo-modal-set-key "fa" 'counsel-ag)

(let ((command
       (cond
	((executable-find "rg")
	 "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
	((executable-find "ag")
	 "ag -i --noheading --nocolor --nofilename --numbers '%s' %s"))))
  (setq counsel-grep-base-command command))

(require 'ivy)
(setq ivy-initial-inputs-alist nil) ;;不需要自动添加^符号
(define-key ivy-minibuffer-map (kbd "C-i") 'ivy-previous-line)
(define-key ivy-minibuffer-map (kbd "C-k") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "M-o") 'ivy-occur)


(require 'find-file-in-project)




(provide 'init-ivy)
