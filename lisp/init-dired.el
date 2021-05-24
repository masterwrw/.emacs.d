(add-package-path '("emacs-async" "w32-browser"))


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
  (dired-hide-details-mode 1)		;隐藏以.开头的文件
  (dired-omit-mode 1)			;隐藏.和..本身 @see https://stackoverflow.com/questions/43628315/how-to-hide-one-dot-current-directory-in-dired-mode
  )

;;(add-hook 'dired-mode-hook 'eye-dired-mode-setup)

(auto-require
 'async
 :load t
 :urls '(("async" . "https://github.com/jwiegley/emacs-async.git"))
 :after
 (progn
   ;; 目录有变化时及时更新
   (setq dired-async-mode 1)
   ))

(require 'wdired)
(require 'dired-x) ;; 支持 dired-jump 进入后自动定位到当前文件名位置


;; 打开 .dired 后缀文件时，自动进入 dired-virtual-mode 模式。
(setq auto-mode-alist (cons '("[^/]\\.dired$" . dired-virtual-mode)
			    auto-mode-alist))

(add-hook 'imenu-after-jump-hook (lambda () (recenter 0)))

(define-key dired-mode-map (kbd "<f12>s") 'dired-dotfiles-toggle)

;; 使用 windows 程序打开文件
;;	(when is-windows
;;	 (require 'w32-browser))

(provide 'init-dired)
