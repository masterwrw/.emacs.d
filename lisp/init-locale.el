(setq user-full-name "owensys")
(setq user-mail-address "owensys@hotmail.com")

(if is-windows
    (progn
      (setq locale-notebook-dir "x:/orgnotes")
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
		  "D:/emacs_env"
		  "D:/emacs_env/texinfo-6.7-w32-bin/bin"
		  "D:/emacs_env/gnuwin32/bin"
		  "d:/emacs_env/emacs-27.2-i686/bin"
		  "D:/emacs_env/curl"
		  "D:/emacs_env/ctags-2017-10-14_d9944ef9-x64"
		  "D:/emacs_env/ripgrep-12.1.1-x86_64-pc-windows-msvc"
		  "D:/emacs_env/ag-2020-07-05_2.2.0-58-g5a1c8d8-x64"
		  "D:/emacs_env/Graphviz/bin"
		  "D:/emacs_env/PortableGit"
		  "D:/emacs_env/PortableGit/bin"
		  ;;msys2, python和eaf用的python冲突，先不设置msys		  
		  ;;"D:/msys64/usr/bin"
		  ;;"D:/msys64/mingw32/bin"
		  "D:/jdk/jre1.8.0_241/bin"
		  "D:/emacs_env/newlisp"
		  "D:/emacs_env/ImageMagick-7.0.10-34-portable-Q16-HDRI-x64"
		  "D:/emacs_env/hugo_0.83.1_Windows-64bit"
		  "D:/emacs_env/Python3.8.5"
		  "D:/emacs_env/Python3.8.5/Scripts"
		  "D:/emacs_env/node-v13.14.0-win-x64"
		  "D:/emacs_env/home/.emacs.d/bin/sqlite-tools-win32-x86-3350500"
		  "D:/portable/cmake-3.18.1-win32-x86/bin"
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

(defun async-shell-command-no-window (command)
  (let ((display-buffer-alist
         (list (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil))))
		(output-buffer (format "*Async Shell Command*.%s" (random 1000)))
		(error-buffer (format "*Async Shell Command*.%s" (random 1000)))
		)
    (async-shell-command command output-buffer error-buffer)))


(when is-windows
  (async-shell-command-no-window (expand-file-name "bin/windows-keys.ahk" user-emacs-directory)))


(defun eye/run-exe ()
  (interactive)
  (async-shell-command "c:/work/xxx/xxx.exe")
  (delete-other-windows))


(with-eval-after-load 'cc-mode
  (define-key c++-mode-map (kbd "C-<f5>") 'eye/run-exe)
  (define-key c++-mode-map (kbd "<f5>") 'eye/auto-compile))


(provide 'init-locale)
