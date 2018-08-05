;;; init-auto-install --- 另一种自动安装包的方式
(require 'auto-install)

(setq auto-install-directory (concat user-emacs-directory "packages")) ;设置默认的安装目录
(setq auto-install-from-w3m-confirm nil) ;从w3m安装不提醒
(setq auto-install-save-confirm nil)     ;不需要确认保存
(setq auto-install-install-compile nil)  ;默认不编译文件


(provide 'init-auto-install)
