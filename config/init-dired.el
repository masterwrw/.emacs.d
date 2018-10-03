;; 目录有变化时及时更新
(require 'async)
(require 'dired)
(setq dired-async-mode 1)

;; 使用 windows 程序打开文件
(when (eq system-type 'windows-nt)
  (require 'w32-browser)
  (define-key dired-mode-map [f11] 'dired-w32-browser))


(provide 'init-dired)
