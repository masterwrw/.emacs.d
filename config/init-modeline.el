(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  ;; 默认的 buffer-encoding-abbrev 会把 utf-8-dos 直接显示成 dos，这里重新定义，用于显示完整的编码
  (spaceline-define-segment buffer-encoding-abbrev
    "The full `buffer-file-coding-system'."
    (format "%s" buffer-file-coding-system))
  
  (setq spaceline-buffer-encoding-p t)
  (setq spaceline-buffer-encoding-abbrev-p t)
  (setq spaceline-line-column-p t)
  (setq spaceline-line-p nil)
  (setq powerline-default-separator (quote arrow))
  (spaceline-spacemacs-theme))

;; 不显示分隔符
(setq powerline-default-separator nil)

;; 显示光标所在行号和列号
(setq line-number-mode t)
(setq column-number-mode t)

;; 显示时间格式
(setq display-time-24hr-format t)
(setq display-time-format "%Y-%m-%d %H:%M")
(display-time-mode 1)

;; 不显示一些模式名称
(use-package diminish
  :ensure t
  :init
  (diminish 'which-key-mode)
  (diminish 'linum-relative-mode)
  (diminish 'hungry-delete-mode)
  (diminish 'visual-line-mode)
  (diminish 'subword-mode)
  (diminish 'page-break-lines-mode)
  (diminish 'auto-revert-mode)
  (diminish 'rainbow-delimiters-mode)
  (diminish 'rainbow-mode)
  (diminish 'indent-guide-mode)
  (diminish 'org-indent-mode)
  (diminish 'helm-mode))


(provide 'init-modeline)
