(setq user-full-name "owensys")
(setq user-mail-address "owensys@hotmail.com")

(setq locale-notebook-dir "H:/wikinote")
(setq locale-docset-dir "~/software/zeal-portable-0.5.0-windows-x64/docsets")


(setenv "GNUPGHOME" "~/.gpgd")


;; PATH and exec-path
;; @see http://ergoemacs.org/emacs/emacs_env_var_paths.html
(when is-windows
  (let ((path-list
	 '(
	   "C:/software/Emacs26.1/ctags-2019-01-18_5a2b65f8-x64"
	   "C:/software/Emacs26.1/Searcher"
	   "C:/software/Emacs26.1/global663wb/bin"
	   "D:/portable/putty"
	   ;;msys2
	   "D:/msys32/usr/bin"
	   "C:/software/Java/bin"
	   )))
    (setenv "PATH" (mapconcat 'identity path-list ";"))
    (setq exec-path (append path-list (list "." exec-directory)))
    ))


;;; tags
(if is-windows
    ;; git-bash进入相应目录后，执行命令“find . | ctags -e -L -”生成TAGS文件
    (setq locale-system-tags-paths (list "C:/Program Files (x86)/Microsoft SDKs/Windows/v7.1A/Include/TAGS"
					 "C:/Program Files (x86)/Microsoft Visual Studio 14.0/VC/include/TAGS"
					 ;;"C:/Projects/xxx/TAGS"
					 ))
  (setq locale-system-tags-paths nil))



(when is-windows
  (with-eval-after-load 'company-c-headers
    ;; should put it to .dir-locals file
    (add-to-list 'company-c-headers-path-user "../Common")
    (add-to-list 'company-c-headers-path-user "../LibHttp")
    )
  )


(defun eye/run-exe ()
  (interactive)
  (async-shell-command "c:/work/xxx/xxx.exe")
  (delete-other-windows))


(with-eval-after-load 'cc-mode
  (define-key c++-mode-map (kbd "C-<f5>") 'eye/run-exe)
  (define-key c++-mode-map (kbd "<f5>") 'eye/auto-compile))


(provide 'init-locale)
