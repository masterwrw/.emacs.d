(require 'tramp)
(setq password-cache-expiry 360000000)      ;设置密码过期时间，避免每次询问密码

(when is-windows
  (setq tramp-default-method "plink")
  (setq tramp-password-end-of-line "\r\n"))
(when is-linux
  (setq tramp-default-method "ssh"))

;; (find-file "/plink:owensys@192.168.199.131:/home/owensys/.bashrc")


(provide 'init-tramp)
