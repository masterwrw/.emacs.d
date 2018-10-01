(setq electric-pair-pairs '(
							(?\{ . ?\})
							(?\( . ?\))
							(?\[ . ?\])
							(?\" . ?\")
							))
(electric-pair-mode t)
(show-paren-mode 1)

;; Show color of #hex format string.
(use-package rainbow-mode
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode))

(use-package expand-region
  :ensure t
  :bind ("C-q" . er/expand-region)
  )

(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

;; save clipboard contents into kill-ring before replace theme
(setq save-interprogram-paste-before-kill t)

(if (display-graphic-p)
    (progn
      (use-package popup-kill-ring
		:ensure t
		:bind
		("M-y" . popup-kill-ring))))


(use-package wdired
  :ensure t)

;; 打开 .dired 后缀文件时，自动进入 dired-virtual-mode 模式。
(require 'dired-x)
(setq auto-mode-alist (cons '("[^/]\\.dired$" . dired-virtual-mode)
                            auto-mode-alist))

;; 如果开启了全局 global-auto-revert，则 dired-virtual-mode 模式下经常会弹出提示，所以只在编程模式下开启。
(add-hook 'prog-mode-hook
		  '(lambda ()
			 (auto-revert-mode 1)))


(require 'nerdtab)
(setq nerdtab-tab-width 30)
(add-to-list 'nerdtab-regex-blacklist "org-src-fontification")
(add-to-list 'nerdtab-regex-blacklist "TAGS")
(global-set-key (kbd "M-0") 'nerdtab-jump-0)
(global-set-key (kbd "M-1") 'nerdtab-jump-1)
(global-set-key (kbd "M-2") 'nerdtab-jump-2)
(global-set-key (kbd "M-3") 'nerdtab-jump-3)
(global-set-key (kbd "M-4") 'nerdtab-jump-4)
(global-set-key (kbd "M-5") 'nerdtab-jump-5)
(global-set-key (kbd "M-6") 'nerdtab-jump-6)
(global-set-key (kbd "M-7") 'nerdtab-jump-7)
(global-set-key (kbd "M-8") 'nerdtab-jump-8)
(global-set-key (kbd "M-9") 'nerdtab-jump-9)


(provide 'init-minor)
