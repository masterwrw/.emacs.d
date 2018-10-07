;; 目录有变化时及时更新
(require 'async)
(require 'dired)
(setq dired-async-mode 1)

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

(define-key dired-mode-map (kbd "<f12>s") 'dired-dotfiles-toggle)

;; 使用 windows 程序打开文件
(when (eq system-type 'windows-nt)
  (require 'w32-browser)
  (define-key dired-mode-map [f11] 'dired-w32-browser))


(provide 'init-dired)
