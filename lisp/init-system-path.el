;; PATH and exec-path
;; @see http://ergoemacs.org/emacs/emacs_env_var_paths.html
(defvar system-path-list)

(if is-windows
    (setq system-path-list
	  '(
	    "D:/emacs_env/texinfo-6.7-w32-bin/bin"
	    "D:/emacs_env/gnuwin32/bin"
	    "D:/emacs_env/emacs-27.2-i686/bin"
	    "D:/emacs_env/curl"
	    "D:/emacs_env/ctags-2017-10-14_d9944ef9-x64"
	    "D:/emacs_env/ripgrep-12.1.1-x86_64-pc-windows-msvc"
	    "D:/emacs_env/ag-2020-07-05_2.2.0-58-g5a1c8d8-x64"
	    "D:/emacs_env/Graphviz/bin"
	    "D:/emacs_env/PortableGit"
	    "D:/emacs_env/PortableGit/bin"
	    ;;msys2, python和eaf用的python冲突，先不设置msys
	    "D:/jdk/jre1.8.0_241/bin"
	    "D:/emacs_env/newlisp"
	    "D:/emacs_env/ImageMagick-7.0.10-34-portable-Q16-HDRI-x64"
	    "D:/emacs_env/hugo_0.83.1_Windows-64bit"
	    "D:/emacs_env/Python3.8.5"
	    "D:/emacs_env/Python3.8.5/Scripts"
	    "D:/emacs_env/node-v13.14.0-win-x64"
	    "D:/emacs_env/wget-1.21.1-1-win32"
	    )))

(if is-linux
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
	    )))


(setenv "PATH" (mapconcat 'identity system-path-list (if is-windows ";" ":")))
(setq exec-path (append system-path-list (list "." exec-directory)))


(provide 'init-system-path)
