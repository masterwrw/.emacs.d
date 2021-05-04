(setq user-full-name "owensys")
(setq user-mail-address "owensys@hotmail.com")

(if is-windows
    (progn
      (setq locale-notebook-dir "E:/home/Dropbox")
      (setq locale-docset-dir "~/.docsets")
      (setq locale-browser-path "C:/Program Files/Mozilla Firefox/firefox.exe")
      )
  (progn
    (setq locale-notebook-dir "/home/orgnote")
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
		  "D:/portable/ctags-2017-10-14_d9944ef9-x64"
		  "D:/portable/ripgrep-12.1.1-x86_64-pc-windows-msvc"
		  "D:/portable/Graphviz/bin"
		  "D:/portable/PortableGit/bin"
		  ;"D:/portable/Emacs26.1/Searcher"
		  ;"D:/portable/Emacs26.1/global663wb/bin"
		  ;;msys2
		  "D:/msys64/usr/bin"
		  "D:/msys64/mingw32/bin"
		  "D:/jdk/jre1.8.0_241/bin"
		  "D:/portable/WinSCP-5.17.5-Portable"
		  "D:/portable/newlisp"
		  "D:/portable/ImageMagick-7.0.10-34-portable-Q16-HDRI-x64"
		  "D:/portable/hugo_0.83.1_Windows-64bit"
		  ))
  (setenv "PATH" (mapconcat 'identity system-path-list ";"))
  (setq exec-path (append system-path-list (list "." exec-directory)))
  )


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
  (setenv "PATH" (mapconcat 'identity system-path-list ":"))
  (setq exec-path (append system-path-list (list "." exec-directory)))
  )



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

(when is-windows
  (async-shell-command (expand-file-name "bin/windows-keys.ahk" user-emacs-directory) nil nil))


(defun eye/run-exe ()
  (interactive)
  (async-shell-command "c:/work/xxx/xxx.exe")
  (delete-other-windows))


(with-eval-after-load 'cc-mode
  (define-key c++-mode-map (kbd "C-<f5>") 'eye/run-exe)
  (define-key c++-mode-map (kbd "<f5>") 'eye/auto-compile))


(provide 'init-locale)
