;;; init-packages.el --- Custom package manager -*- lexical-binding: t -*-
;;
;; TODO:
;; 1.移除dash, s, f依赖 => ok
;; 2.git clone 增加 --depth=1 => ok
;; 3.把下载包的命令自动添加到shell脚本（或者使用变量保存命令列表）
;; 4.采用异步执行命令
;; 5.增加删除包的函数（删除目录，删除load-path，删除shell脚本中，更新autoloads）
;; 6.启动时调用安装包（判断本地目录是否存在，不存在则异步安装）

(defvar eye-packages-dir (expand-file-name "emacs-packages" "~/src")
  "All packages main directory.")

(defvar eye-git-bin-dir nil "The git directory")
(defvar eye-wget-bin-dir nil "The wget directory")

;; (setq eye-git-bin-dir "d:/msys32/usr/bin")
;; (setq eye-wget-bin-dir "d:/msys32/usr/bin")

(if eye-git-bin-dir
    (progn
      (setenv "PATH" (concat (getenv "PATH") ";" eye-git-bin-dir))
      (add-to-list 'exec-path eye-git-bin-dir)))
(if eye-wget-bin-dir
    (progn
      (setenv "PATH" (concat (getenv "PATH") ";" eye-wget-bin-dir))
      (add-to-list 'exec-path eye-wget-bin-dir)))

;;1.download: git clone / mkdir xx && wget -Oxx/xx.el url
(defun eye-package-download(url path &optional name)
  (let (command)
    (if (string-match "github" url)
	(setq command (concat "git clone --depth=1 " url " " path))
      (progn
	(shell-command (concat "mkdir " path))
	(setq command (concat "wget -O" path "/" name ".el " url))
	))
    (message (concat "download:" command))
    (shell-command command nil)
    ))

(defun eye/package-install (&optional site-url name)
  (interactive)
  (if (executable-find "wget")
      (progn
	(let ((url site-url) (path name) command)
	  (unless url (setq url (read-string "url:")))
	  (unless name (setq name (read-string "package name:")))
	  (setq path (concat eye-packages-dir "/" name))
	  (eye-package-download url path name)
	  (if (file-exists-p path)
	      (progn
		(add-to-list 'load-path path)
		(message "compile")
		(byte-recompile-directory path 0)
		(message "generate autoloads")
		(update-directory-autoloads path)
		(message "install package all finished."))
	    (message "download package failed"))
	  ))
    (message "no wget or git found.")))

;; (eye/package-install "https://github.com/myrjola/diminish.el.git" "diminish")
;; (eye/package-install "http://www.emacswiki.org/emacs/download/anything.el" "anything")
;; (eye/package-install "http://www.emacswiki.org/emacs/download/anything-config.el" "anything-config")


;;2.byte compile directory
;;(byte-recompile-directory "~/packages/youdao-dictionary" nil)
;;(byte-recompile-directory "~/packages/writeroom-mode" nil)
;;(byte-recompile-directory "~/packages/writeroom-mode" nil t) ;force byte compile if existing .elc file
(defun eye/package-compile-all ()
  "编译所有包，如果有很多包没有编译过，由于耗时且卡界面，最好使用命令行方式编译
git-bash进入包目录后执行
/path/of/emacs -q --load ~/.emacs.d/lisp/init-packages.el --batch --eval '(eye/package-compile-all)'
"
  (interactive)
  (byte-recompile-directory eye-packages-dir 0)) ;must use 0 to compile

(defun eye/package-compile-dir ()
  "Compile a directory"
  (interactive)
  (let ((path (ido-read-directory-name "Select dir:")))
    (when (and path (file-exists-p path))
      (message "Compile %s" path)
      (byte-recompile-directory path 0))))

;;3.generate autoload file
;;(update-directory-autoloads "~/packages/youdao-dictionary")
;;(update-directory-autoloads "~/packages/writeroom-mode")
(defun eye/package-update-autoload ()
  (interactive)
  (let ((name (read-string "package name:"))
	path)
    (when name
      (setq path (concat eye-packages-dir "/" name))
      (if (file-exists-p path)
	  (message "Update autoloads: %s" name)
	  (update-directory-autoloads path)))))

;; traverse dir @see http://ergoemacs.org/emacs/elisp_traverse_dir.html
(defun eye/package-update-autoload-all ()
  (interactive)
  ;; delete autoload file
  (if (file-exists-p generated-autoload-file)
      (delete-file generated-autoload-file t))
  ;; update all subdir autoloads of packages directory
  (mapc (lambda (x)
	  (let ((path (expand-file-name x eye-packages-dir)))
	    (when (and (file-exists-p path)
		       (not (string-equal x "."))
		       (not (string-equal x "..")))
	      (update-directory-autoloads path)
	      )))
	(directory-files eye-packages-dir)))


;;4.upgrade package: cd ~/packages/xxx && git pull then force compile
(defun eye/package-force-compile (&optional package-path)
  (interactive)
  (let ((path package-path))
    (unless path (setq path (read-string "package dir name:")))
    (setq path (concat eye-packages-dir "/" path))
    (when (file-exists-p path)
      (byte-recompile-directory path 0 t))))


;;5.remove package: rm -rf ~/packages/xx
(defun eye/package-delete (&optional name)
  (interactive)
  (let (path name)
    (unless name (setq name (read-string "package name (dir):")))
    (setq path (concat eye-packages-dir "/" name))
    (if (file-exists-p path)
	(progn
	  (delete-directory path t t) ;force delete recursively
	  (message "delete package finished."))
      (message "package not exists."))))


(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))


;; autoload file
(setq generated-autoload-file (expand-file-name "autoload.pkg.el" eye-packages-dir))

(defvar autoload-file-exist (file-exists-p generated-autoload-file))
(defun require-maybe (feature)
  (unless autoload-file-exist (require feature)))

;;; require autoload.pkg file
;; 1.由于一些包中有eval-when-compile代码，生成的autoload文件中有require其它包，必须先添加到load-path
;; 2.require autoload文件后不需要手动require，可以加快启动，但是有的包函数会自动出现在M-x中
;; 可能是由于autoload信息文件中出现了register-definition-prefixed导致，比如：
;; (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "with-editor/with-editor" '("with-editor" "start-file-process--with-editor-process-filter" "server-" "shell-command--shell-command-with-editor-mode")))
;; (eye--reset-time) ;;使用命令行编译时无法找到此函数定义，先注释
(add-subdirs-to-load-path eye-packages-dir) ;; all site packages don't put to .emacs.d
(add-subdirs-to-load-path (concat user-emacs-directory "lisp/"))
;; (eye--print-time "add packages path")

;; (eye--reset-time)
(setq test-time (current-time))

(when autoload-file-exist
  (require 'autoload.pkg generated-autoload-file))

(message
 (format "%.6f sec: require autoload.pkg"
	 (- (float-time (current-time))
	    (float-time test-time))))

;; (eye--print-time "require autoload.pkg")



(provide 'init-packages)
;;; init-packages.el ends here
