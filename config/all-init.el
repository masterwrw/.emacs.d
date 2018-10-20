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



;; 防止退出时卡死在 Saving clipboard to X clipboard manager 状态
(setq x-select-enable-clipboard-manager nil)


(setq inhibit-startup-message t) ;; 禁用启动后显示的消息 buffer
(setq initial-scratch-message nil) ;; 禁止显示 *scratch* buffer 中默认出现的文本
(put 'suspend-frame 'disabled t) ;; 禁止 Ctrl+z 时挂起 emacs

;; 用 y/n 代替 yes/no 的回答
;; (fset 'yes-or-no-p 'y-or-n-p) 相同的效果
(defalias 'yes-or-no-p 'y-or-n-p)

(setq ring-bell-function 'ignore) ;; 禁止出现烦人的响铃


(defvar user-cache-directory (expand-file-name "~/tmp/emacs_cache/"))
(unless (file-exists-p user-cache-directory)
  (mkdir user-cache-directory))

;; 禁止自动写入一些内容到 init.el 文件后面
;; https://www.reddit.com/r/emacs/comments/4q4ixw/how_to_forbid_emacs_to_touch_configuration_files/
(setq custom-file (concat user-cache-directory "custom-set-variables.el"))
(load custom-file 'noerror)

;;; 开始初始化配置
;;(with-temp-message "" ;;不显示启动时的输出
(require 'init-benchmark)
(require 'init-encoding)
(require 'init-backup)
(require 'init-history)
(require 'init-shell)
(require 'base-toolkit)
(require 'init-ryo)
(require 'init-ivy)
(require 'session-init)

;; 延后加载
(require 'idle-require)
(setq idle-require-idle-delay 1)

(setq idle-require-symbols
      '(init-gui
		init-theme
		init-modeline
		auto-save-init
		window-init
		init-edit
		init-dired
		init-git
		init-org
		org-wiki-init
		init-company-mode
		init-python
		init-cpp
		init-elisp
		init-php
		init-flycheck
		init-sql
		init-navigation
		init-yasnippet
		init-tramp
		init-document
		init-external
		custom-keys
		))
(idle-require-mode 1) ;; start loading


(provide 'all-init)
