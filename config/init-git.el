(require 'magit)
(setq magit-push-always-verify nil)
(setq git-commit-summary-max-length 80)

;; 在新 frame 中打开 magit-status
(defun magit-display-buffer-pop-up-frame (buffer)
  (if (with-current-buffer buffer (eq major-mode 'magit-status-mode))
      (display-buffer buffer
                      '((display-buffer-reuse-window
                         display-buffer-pop-up-frame) ;; 在新的 frame 中显示
                        (reusable-frames . t)))
    (magit-display-buffer-traditional buffer))) ;; magit-display-buffer-traditional 是默认的函数

;; 设置显示 magit buffer 的函数
(setq magit-display-buffer-function #'magit-display-buffer-pop-up-frame)

(define-key magit-mode-map (kbd "q") 'delete-frame) ;; 自动关闭 frame

(define-key global-map (kbd "<f7> g") 'magit-status)

;; 添加 magit-submodule-remove
(require 'magit-extension)






(provide 'init-git)
