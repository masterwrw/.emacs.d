(setq user-full-name "owensys")
(setq user-mail-address "owensys@hotmail.com")

(if is-windows
    (progn
      (setq locale-notebook-dir "F:/wikinote")
      (setq locale-docset-dir "~/.docsets")
      (setq locale-browser-path "C:/Program Files (x86)/Maxthon5/Bin/Maxthon.exe")
      )
  (progn
    (setq locale-notebook-dir "/home/dev/orgnote")
    (setq locale-browser-path "/usr/bin/firefox")
    (setq locale-docset-dir "~/.docsets")
    ))
    


;; (setenv "GNUPGHOME" "~/.gpgd")


;; PATH and exec-path
;; @see http://ergoemacs.org/emacs/emacs_env_var_paths.html
(defvar system-path-var nil)
(when is-windows
  (setq system-path-list
	'(
	  "D:/portable/Emacs26.1/ctags-2019-01-18_5a2b65f8-x64"
	  "D:/portable/Emacs26.1/Searcher"
	  "D:/portable/Emacs26.1/global663wb/bin"
	  "D:/portable/putty"
	  ;;msys2
	  "D:/msys32/usr/bin"
	  "C:/software/Java/bin"
	  "D:/portable/WinSCP-5.15.3-Portable"
	  "D:/portable/Emacs26.1/newlisp"
	  ))
  (setenv "PATH" (mapconcat 'identity system-path-list ";")))

(when is-linux
  (setq system-path-list
	'(
	  "/usr/local/sbin" "/usr/local/bin" "/usr/sbin" "/usr/bin" "/sbin" "/bin" "/usr/games" "/usr/local/games"
	  "/home/owen/opt"
	  "/home/owen/opt/emacs-26.3/bin"
	  "/home/owen/opt/universal-ctags/bin"
	  "/home/owen/src/emacs-packages/fuz"
	  "/home/owen/opt/oracle/home/bin"
	  "/home/owen/opt/xapian/bin"
	  "/home/dev/opt/ccls/Release"
	  ))
  (setenv "PATH" (mapconcat 'identity system-path-list ":")))


(setq exec-path (append system-path-list (list "." exec-directory)))



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
