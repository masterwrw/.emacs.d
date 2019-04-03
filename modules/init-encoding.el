;;;; Encoding
(setq locale-coding-system 'utf-8)     ;; 设置emacs 使用 utf-8
(set-language-environment 'Chinese-GB) ;; 设置为中文简体语言环境
(set-keyboard-coding-system 'utf-8)    ;; 设置键盘输入时的字符编码
;; 解决粘贴中文出现乱码的问题
(if (eq system-type 'windows-nt)
    (progn
      ;; (setq selection-coding-system 'utf-16le-dos) ;; 修复从网页剪切文本过来时显示 \nnn \nnn 的问题
      ;; (set-default selection-coding-system 'utf-16le-dos)
      (set-selection-coding-system 'utf-16le-dos) ;; 别名set-clipboard-coding-system
      )
  (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)
;; 文件默认保存为 utf-8
(set-buffer-file-coding-system 'utf-8)
(set-default buffer-file-coding-system 'utf8)
(set-default-coding-systems 'utf-8)
;; 防止终端中文乱码
(set-terminal-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))
;; 解决文件目录的中文名乱码
(setq-default pathname-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)

;; windows shell
(when (and is-windows is-terminal)
  (defun eye/change-shell-mode-coding ()
    (progn
      (set-terminal-coding-system 'gbk)
      (set-keyboard-coding-system 'gbk)
      ;; (set-selection-coding-system 'gbk)
      (set-buffer-file-coding-system 'gbk)
      (set-file-name-coding-system 'gbk)
      (modify-coding-system-alist 'process "*" 'gbk)
      (set-buffer-process-coding-system 'gbk 'gbk)
      (set-file-name-coding-system 'gbk)))
  (add-hook 'shell-mode-hook 'eye/change-shell-mode-coding)
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

(defun eye/convert-to-utf8-unix ()
  (interactive)
  (set-buffer-file-coding-system 'unix 't))

;; vs2015默认文件编码
(defun eye/convert-to-chinese-iso-8bit-dos ()
  (interactive)
  (set-buffer-file-coding-system 'chinese-iso-8bit-dos 't))

;; org笔记编码
(defun eye/convert-to-utf-8-withsignature-unix ()
  (interactive)
  (set-buffer-file-coding-system 'utf-8-with-signature-unix 't))



(provide 'init-encoding)
