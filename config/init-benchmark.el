;;
;; 启动时间统计
;;
;; 另外有一个内置函数 init-emacs-time 可以知道大概启动时间

(use-package benchmark-init
  :ensure t
  :config
  (benchmark-init/activate)
  :hook
  (after-init . benchmark-init/deactivate))

;; 自定义计算时间
(defvar init-start (current-time))
(add-hook 'after-init-hook
          (lambda ()
            (message (format "Init completed in %.6fs\n\n" (- (float-time (current-time)) (float-time init-start))))
            ))


(provide 'init-benchmark)
