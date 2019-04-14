;;(require 'auto-save)
;;(auto-save-enable)
;;(setq auto-save-silent t)
;; (setq auto-save-delete-trailing-whitespace t)

(require 'youdao-dictionary)

(autoload 'mpg123 "mpg123" "A Front-end to mpg123/ogg123" t)

;; (setq show-paren-style 'expression)     ;高亮括号整体内容


;; fix warning: ad-handle-definition: ‘er/expand-region’ got redefined
;; (setq ad-redefinition-action 'accept)
;; (use-package expand-region
  ;; :ensure t
  ;; :bind ("C-q" . er/expand-region)
  ;; )

;; 如果开启了全局 global-auto-revert，则 dired-virtual-mode 模式下经常会弹出提示，所以只在编程模式下开启。
(add-hook 'prog-mode-hook
                  '(lambda ()
                         (auto-revert-mode 1)))

;;;; remote
(if is-windows)
    (setq tramp-default-method "plink")
  (setq tramp-default-method "ssh"))

;; 不设置为全局,否则影响minibuffer输入
;; (define-key prog-mode-map (kbd "<tab>") 'indent-or-expand)
(define-key prog-mode-map (kbd "<tab>") 'hippie-expand)



(provide 'init-modules)
