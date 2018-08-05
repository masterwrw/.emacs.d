;;; init-begin --- 配置开始前的一些设置

;;; 加快启动
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defvar eye-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun eye/revert-file-name-handler-alist ()
  (setq file-name-handler-alist eye-file-name-handler-alist))

(defun eye/reset-gc ()
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook 'eye/revert-file-name-handler-alist)
(add-hook 'emacs-startup-hook 'eye/reset-gc)


;;; 设置 require 路径
(eval-when-compile (require 'cl))
(defun eye/set-load-path (full-path-dir)
  (if (fboundp 'normal-top-level-add-to-load-path)
      (let* ((my-lisp-dir full-path-dir)
	     (default-directory my-lisp-dir))
	(progn
          (setq load-path
		(append
		 (loop for dir in (directory-files my-lisp-dir)
                       unless (string-match "^\\." dir)
                       collecting (expand-file-name dir))
		 load-path)))))
  )

(defun eye/update-load-path ()
  (interactive)
  (eye/set-load-path (concat user-emacs-directory "site-lisp")))

(eye/update-load-path)


;; 防止退出时卡死在 Saving clipboard to X clipboard manager 状态
(setq x-select-enable-clipboard-manager nil)



(provide 'init-begin)
