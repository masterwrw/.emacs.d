(require 'mule)

;;(setq locale-coding-system 'utf-8)     ;; 设置emacs 使用 utf-8
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

;; set coding config, last is highest priority.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Recognize-Coding.html#Recognize-Coding
(prefer-coding-system 'cp950)
(prefer-coding-system 'gb2312)
(prefer-coding-system 'cp936)
(prefer-coding-system 'gb18030)
(prefer-coding-system 'utf-16)
(prefer-coding-system 'utf-8-dos)
(prefer-coding-system 'utf-8-unix)

;; 文件默认保存为 utf-8
(set-buffer-file-coding-system 'utf-8-unix)
(set-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
;; 防止终端中文乱码
(set-terminal-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))
;; 解决文件目录的中文名乱码
(setq-default pathname-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)

(when is-windows
  (setq default-process-coding-system '(gbk . gbk))
  ;; file encoding
  ;; @see https://www.gnu.org/software/emacs/manual/html_node/emacs/Recognize-Coding.html
  ;;(modify-coding-system-alist 'file "\\.txt\\'" 'chinese-iso-8bit-dos)
  ;;(modify-coding-system-alist 'file "\\.h\\'" 'chinese-iso-8bit-dos)
  ;;(modify-coding-system-alist 'file "\\.cpp\\'" 'chinese-iso-8bit-dos)
  )

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
      (set-buffer-process-coding-system 'gbk 'gbk)))
  (add-hook 'shell-mode-hook 'eye/change-shell-mode-coding)
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))


(provide 'init-encoding)

