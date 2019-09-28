(require 'tramp)
(setq password-cache-expiry 360000000)      ;设置密码过期时间，避免每次询问密码

(when is-windows
  (setq tramp-default-method "plink")
  (setq tramp-password-end-of-line "\r\n"))
(when is-linux
  (setq tramp-default-method "ssh"))

;; (find-file "/plink:owensys@192.168.199.131:/home/owensys/.bashrc")


;; (setq filepath "e:/@192.168.199.128_owensys/home/owensys/tmp/main.cpp")


;; (setq server-list '(("192.168.199.128_owensys" "testpwd")
;; 		    ("192.168.199.131_oracle" "testpwd2")))

;; (setq selserver (completing-read "Select server: " server-list))

;; (dolist (sv server-list)
;;   (if (string-equal selserver (car sv))
;;       (let ((pwd (car (cdr sv)))
;; 	    (prepath (concat "/@" selserver)))
;; 	(message (format "%s" prepath))
;; 	)))


(defun remote-file ()
  (interactive)
  (let* ((filepath (read-file-name "Remote path:" "/@ip_user/"))
	 ;; (filepath "/@192.168.199.128_owensys/home/owensys/tmp/main.cpp")
	 (localpath (expand-file-name filepath))
	 (localdir (replace-regexp-in-string "/" "\\\\" (file-name-directory localpath)))
	 (scriptfile (expand-file-name "~/tmp/myscp.bat"))
	 ip username iplen)
    (with-temp-buffer
      (setq ip (s-match "/@[0-9.]+_" filepath))
      (setq ip (car ip))
      (setq ip (replace-regexp-in-string "/@" "" ip))
      (setq ip (replace-regexp-in-string "_" "" ip))
      (setq iplen (+ 3 (length ip))) ;;iplen+length of /@_
      (setq username (substring filepath iplen (string-match "/" filepath iplen)))
      (setq filepath (substring filepath (+ iplen (length username)) (length filepath)))
      ;; 需要先创建本地目录
      (if (not (file-directory-p (file-name-directory localpath)))
	  (shell-command (concat "mkdir -p " (file-name-directory localpath))))
      
      (insert (concat "option confirm off\n"
		      "open " username ":owenowen@" ip ":22\n"
		      "get " filepath " " localdir "\n"
		      "close\n"
		      "exit\n"
		      ))
      (write-file scriptfile)
      (shell-command (concat "WinSCP.com /script=" (replace-regexp-in-string "/" "\\\\" scriptfile)))
      (if (file-exists-p localpath)
	  (find-file localpath))
     )))

(defun remote-save ()
  (interactive)
  (save-buffer)
  (let* ((filepath (expand-file-name (buffer-file-name)))
	 (scriptfile (expand-file-name "~/tmp/myscp.bat"))
	 ip username iplen remotedir)
    (with-temp-buffer
      (setq ip (s-match "[a-z]:/@[0-9.]+_" filepath))
      (setq ip (car ip))
      (setq ip (replace-regexp-in-string "[a-z]:/@" "" ip))
      (setq ip (replace-regexp-in-string "_" "" ip))
      (setq iplen (+ 5 (length ip))) ;;iplen+length of /@_
      (setq username (substring filepath iplen (string-match "/" filepath iplen)))
      (setq remotedir (substring filepath (+ iplen (length username)) (length filepath)))
      (setq filepath (replace-regexp-in-string "/" "\\\\" filepath))
      (insert (concat "option confirm off\n"
		      "open " username ":owenowen@" ip ":22\n"
		      "put " filepath " " remotedir "\n"
		      "close\n"
		      "exit\n"
		      ))
      (write-file scriptfile)
      (shell-command (concat "WinSCP.com /script=" (replace-regexp-in-string "/" "\\\\" scriptfile)))
      (delete-file scriptfile)
     )))



(provide 'init-tramp)
