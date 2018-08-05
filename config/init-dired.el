;; 目录有变化时及时更新
(use-package async
  :ensure t
  :init
  (dired-async-mode 1))

;; 使用 windows 程序打开文件
(when (eq system-type 'windows-nt)
  (use-package w32-browser
    :ensure t
    :config
    (define-key dired-mode-map [f11] 'dired-w32-browser)))



(provide 'init-dired)
