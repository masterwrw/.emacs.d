(require-maybe 'dired)


(defun dired-dotfiles-toggle ()
  "Show/hide dot-files"
  (interactive)
  (when (equal major-mode 'dired-mode)
        (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
                (progn 
                  (set (make-local-variable 'dired-dotfiles-show-p) nil)
                  (message "h")
                  (dired-mark-files-regexp "^\\\.")
                  (dired-do-kill-lines))
          (progn (revert-buffer) ; otherwise just revert to re-show
                         (set (make-local-variable 'dired-dotfiles-show-p) t)))))


;; 隐藏 dired 中文件拥有者和文件权限等信息
(defun eye-dired-mode-setup ()
  "hide the file's unix owner and permission info"
  (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook 'eye-dired-mode-setup)



(with-eval-after-load 'dired
  (require 'wdired)
  (require 'dired-x) ;; 支持 dired-jump 进入后自动定位到当前文件名位置
  (require 'async) ;; 目录有变化时及时更新
  (setq dired-async-mode 1)

  ;; 打开 .dired 后缀文件时，自动进入 dired-virtual-mode 模式。
  (setq auto-mode-alist (cons '("[^/]\\.dired$" . dired-virtual-mode)
                              auto-mode-alist))

  
  (define-key dired-mode-map (kbd "<f12>s") 'dired-dotfiles-toggle)

  ;; 使用 windows 程序打开文件
  ;; (when is-windows
    ;; (require 'w32-browser)
    ;; (define-key dired-mode-map [f11] 'dired-w32-browser))

  
  )



(defhydra+ hydra-file (:exit t :idle 1.0)
  ("d" dired-jump "Dired"))



(provide 'init-dired)
