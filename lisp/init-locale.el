(eye--reset-time)

(setq locale-notebook-dir "~/notebook/org/note")
(setq locale-notebook-attachment-dir "~/notebook/attach")
(setq locale-gtd-dir "~/notebook/org/gtd")
(setq locale-password-file "~/notebook/org/password.org")
(setq locale-custom-projects (list (concat locale-gtd-dir "/project/ud.org")
				   (concat locale-gtd-dir "/project/supertool.org")
				   ))
(setq locale-docset-dir "~/software/zeal-portable-0.5.0-windows-x64/docsets")


;; PATH and exec-path
;; @see http://ergoemacs.org/emacs/emacs_env_var_paths.html
(when is-windows
  (let ((path-list
	 '(
	   "C:/software/Emacs26.1/ctags-2019-01-18_5a2b65f8-x64"
	   "C:/software/Emacs26.1/Searcher"
	   "C:/Users/soeye/.babun/cygwin/bin"
	   "C:/software/Emacs26.1/global663wb/bin"
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



(defun eye/open-goals-file ()
  (interactive)
  (find-file-existing (concat locale-gtd-dir "/goals.org")))


(defun eye/open-locale-file ()
  (interactive)
  (find-file locale-config-file))



(when is-windows
  (with-eval-after-load 'company-c-headers
    ;; should put it to .dir-locals file
    (add-to-list 'company-c-headers-path-user "../Common")
    (add-to-list 'company-c-headers-path-user "../LibHttp")
    )
  )




(eye--print-time "load locale")




(provide 'init-locale)
