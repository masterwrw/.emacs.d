;;;; etags
(defun eye/create-ctags-file ()
  "Create ctags file(windows ok)"
  (interactive)
  ;; ctags必须加-e选项，否则counsel-xxx-find-tag-xx无法识别其中的tagname
  (let ((tags-dir (ido-read-directory-name "TAGS DIR:"))
	;; 需要传"\\("，否则出现错误：bash: -c:行0: 未预期的符号 `(' 附近有语法错误
	(command "find %s \\( -iwholename \"*.h\" -or -iwholename \"*.cpp\" \\) -print | ctags -e -f %sTAGS -V -R -L -"))
    (setq command (format command tags-dir tags-dir))
    (message command)
    (let ((proc (start-process "ctags" nil shell-file-name shell-command-switch command)))  ;; shell-command-switch值为-c，表示后面的是命令行参数
      (set-process-sentinel proc `(lambda (proc msg)
				    (let ((status (process-status proc)))
				      (when (memq status '(exit signal))
					(message "ctags:%s" msg)
					)))))
    ;;    (async-shell-command command)
    ))

(defun eye/create-ctags-file-by-git ()
  "在项目目录下执行此命令，即可生成TAGS文件，如果弹出提示时，需要选yes"
  (interactive)
  (async-shell-command "git ls-tree -r HEAD --name-only > list_files")
  (with-temp-buffer
    (insert "--output-format=etags\r\n")
    (insert "-f TAGS\r\n")
    (insert "-L list_files\r\n")
    (write-file "opt.ctags"))
  (async-shell-command (concat (executable-find "ctags") " --options=opt.ctags")))


(defun eye/update-ctags-this-file ()
  "Update current file tags"
  (interactive)
  (let ((tags-path (locate-dominating-file default-directory "TAGS"))
	(command)
	(proc))
    (when tags-path
      (setq tags-path (expand-file-name "TAGS" tags-path))
      (setq command (format "ctags -e -a -f %s %s" tags-path (buffer-name))) ;; -a means append
      (message (concat "custom command:" command))
      (async-shell-command command)
      (delete-other-windows))))

;; company-etags要么使用当前项目的TAGS，要么使用tags-table-list定义的TAGS文件，所以干脆直接配置tags-table-list
;;(if locale-system-tags-paths
;;    (append-to-list 'tags-table-list locale-system-tags-paths)) ;; need load init-locale
;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)
;; Do case-sensitive tag searches
(setq tags-case-fold-search nil) ;; t=case-insensitive, nil=case-sensitive
;; Don't warn when TAGS files are large
(setq large-file-warning-threshold nil)

(defun eye/load-project-root-tags ()
  "加载本地文件上层目录中对应的TAGS文件"
  (interactive)
  (let ((root-tags-file (locate-dominating-file default-directory "TAGS")))
    (when root-tags-file
      (setq root-tags-file (concat root-tags-file "TAGS"))
      (message "Loading tags file: %s" root-tags-file)
      (visit-tags-table root-tags-file)
      (add-to-list 'tags-table-list root-tags-file)
      (add-to-list 'tags-table-files root-tags-file) ;; for find-file-in-tags
      )))



;;;; counsel-etags
(auto-require
 'counsel-etags
 :urls '(("counsel-etags" . "https://github.com/redguardtoo/counsel-etags.git"))
 :paths "counsel-etags"
 :functions '(counsel-etags-find-tag counsel-etags-find-tag-at-point)
 :after
 (progn
   ;; Don't ask before rereading the TAGS files if they have changed
   (setq tags-revert-without-query t)
   ;; Don't warn when TAGS files are large
   (setq large-file-warning-threshold nil)
   (when is-linux
     (setq counsel-etags-tags-program "xargs etags --append") ;调用命令类似 find .... -print | xargs etags --append, etags没有递归的参数
     )
   (when is-windows (setq counsel-etags-tags-program (executable-find "ctags"))) ;; if not set, (counsel-etags-guess-program "ctags") find failed

   ;; 是否开启输出命令
   (setq counsel-etags-debug nil)

   ;;(append-to-list 'counsel-etags-extra-tags-files locale-system-tags-paths) ;;使counsel-etags能显示系统函数（但无法跳转进入）
   
   ;; Setup auto update now
   (setq counsel-etags-update-tags-backend 'eye/update-ctags-this-file)  
   (add-hook 'c++-mode-hook
	     (lambda ()
	       (add-hook 'after-save-hook 'counsel-etags-virtual-update-tags 'append 'local)
	       ;;(add-hook 'after-save-hook 'eye/update-ctags-this-file))
	       ))
   
   ;; counsel-etags-ignore-directories does NOT support wildcast
   (dolist (dirname (list ".git" ".svn" ".vs" "ipch" "Debug" "Release" "Bin" "tmp"))
     (add-to-list 'counsel-etags-ignore-directories dirname))

   ;; counsel-etags-ignore-filenames supports wildcast
   (dolist (filename (list "TAGS" "GPATH" "GTAGS" "*.json" "ui_*.h" "*.ui" "moc_*.cpp" "*.rc"
			   "*.qrc" "*.tlog" "*.md" "*.bat" "*.txt" "*.pdb" "*.filters" "*.user"
			   "*.vcproj" "*.vcxproj" "*.db" "*.opendb" "*.htm" "*.user" "*.make"
			   "*.sln" "*.exp" "*.sdf" "*.opensdf"))
     (add-to-list 'counsel-etags-ignore-filenames filename))
   ))


;;;; company
;;; 对不同的major-mode设置不同的company-backends
;; 1.设置(set 'company-backend xxx)切换buffer后又会变为空，即使设置了也不能自动弹出TAGS选项，所以改为使用
;; 2.设置after-change-major-mode-hook后，切换buffer时会得到major-mode为minibuffer-inactive-mode，不能得到切换后buffer的major-mode
;; 3.add-hook majoe-mode hook也不能检测到切换buffer后的major-mode，要重新打开buffer才能调用到hook
;; 4.add company-mode-hook 修改company-backends, add major-mode hook开启company-mode（非global），问题：也不能检测到切换buffer后的major-mode
;; 5.add global-company-mode-hook根据major-mode修改company-backends，切换buffer后，重新开启global-company-mode
;; 6.最终选择：手动开启global-company-mode，根据major-mode修改company-backends，当切换buffer后，重新开启global-company-mode，效果同第5条
(defun buffer-mode (&optional buffer-or-name)
  "Returns the major mode associated with a buffer.
If buffer-or-name is nil return current buffer's mode."
  (buffer-local-value 'major-mode
		      (if buffer-or-name (get-buffer buffer-or-name) (current-buffer))))

(defun eye-set-major-mode-backends (backends modmap)
  "设置company-backends并设置相应major mode map下的按键，由于define-key第一个参数不能是symbol，需要在hook函数中使用"
  (message "Change company-backends to: %s" backends)
  (setq company-backends backends)
  ;;(set (make-local-variable 'company-backends) backends) ;;不能使用setq设置local variable 'company-backend
  (when (keymapp modmap)
    (define-key modmap (kbd "<backtab>") 'company-manual-begin)
    (define-key modmap (kbd "<S-tab>") 'company-manual-begin)))

(defun eye-setup-company-backends ()
  "Setup company-backends for major-mode"
  (when (or (bound-and-true-p global-company-mode) (bound-and-true-p company-mode))
    (cond ((eq major-mode 'css-mode)
	   (eye-set-major-mode-backends
	    '((company-css company-abbrev company-dabbrev company-dabbrev-code company-keywords))
	    css-mode-map))
	  ((eq major-mode 'c-mode)
	   (eye-set-major-mode-backends
	    '((company-etags company-abbrev company-dabbrev company-dabbrev-code company-keywords))
	    c-mode-map))
	  ((eq major-mode 'c++-mode)
	   (eye-set-major-mode-backends
	    '((company-etags company-abbrev company-dabbrev company-dabbrev-code company-keywords))
	    c++-mode-map))
	  ((eq major-mode 'emacs-lisp-mode)
	   (eye-set-major-mode-backends
	    '((company-elisp company-abbrev company-files company-dabbrev company-dabbrev-code company-keywords))
	    emacs-lisp-mode-map))
	  ((eq major-mode 'lisp-interaction-mode)
	   (eye-set-major-mode-backends
	    '((company-elisp company-abbrev company-files company-dabbrev company-dabbrev-code company-keywords))
	    lisp-interaction-mode-map))
	  ((equal major-mode 'nxml-mode)
	   (eye-set-major-mode-backends
	    '((company-nxml company-abbrev company-dabbrev company-dabbrev-code))
	    nxml-mode-map))
	  ((equal major-mode 'php-mode)
	   (eye-set-major-mode-backends
	    '((company-php company-abbrev company-dabbrev company-dabbrev-code))
	    php-mode-map))
	  (t (eye-set-major-mode-backends
	      '((company-abbrev company-dabbrev company-dabbrev-code company-files))
	      global-map))
	  )))


(defun eye/company-remove-dups (candidates)
  "自动移除company列表的中重复元素"
  (let ((newseq))
    (mapcar #'(lambda (c)
		(if (not (member c newseq))
		    (add-to-list 'newseq c)))
	    candidates)
    newseq))

(auto-require
 'company
 :load t
 :urls '(("company-mode" . "https://github.com/company-mode/company-mode.git"))
 :paths "company-mode"
 :after
 (progn
   (setq company-idle-delay 0.2)
   (setq company-minimum-prefix-length 3)
   (setq company-show-numbers t)
   (setq company-echo-delay 0)
   (setq company-require-match nil)
   ;; make previous/next selection in the popup cycles
   (setq company-selection-wrap-around t)
   ;; aligns annotation to the right hand side
   (setq company-tooltip-align-annotations t)
   ;; bigger popup window
   (setq company-tooltip-limit 20)
   ;;(set-face-attribute 'company-tooltip nil :foreground "magenta")
   
   (setq company-dabbrev-code-everywhere t) ;;Non-nil to offer completions in comments and strings.
   (setq company-dabbrev-minimum-length 3) ;;The minimum length for the completion candidate to be included.
   (setq company-dabbrev-other-buffers t) ;;If t, 'company-dabbrev' search buffers with the same major mode
   (setq company-dabbrev-downcase nil) ;;是否把候选项前面的字母转为小写
   (setq company-dabbrev-ignore-case t) ;;使选择后，前面输入的字母也转为正确的大小写
   (setq company-dabbrev-char-regexp "[\\.0-9a-z-_'/]") ;adjust regexp make `company-dabbrev' search words like `dabbrev-expand'

   (setq company-dabbrev-code-ignore-case t)
   (setq company-dabbrev-code-other-buffers 't) ;If t, search buffers with the same major mode.

   (setq company-etags-ignore-case t)
   (setq company-etags-support-ctags-only t)
   (add-to-list 'company-transformers #'eye/company-remove-dups) 
   ;; set default backends
   ;; company-dabbrev is for current buffer string auto complete
   (setq company-backends '(company-abbrev company-dabbrev company-dabbrev-code company-files))
   (add-hook 'global-company-mode-hook 'eye-setup-company-backends)
   ))

;; Support yas in commpany
;; Note: Must be the last to involve all backends
;; (defvar company-enable-yas t
;;   "Enable yasnippet for all backends.")

;; (defun company-backend-with-yas (backend)
;;   (if (or (not company-enable-yas)
;;           (and (listp backend) (member 'company-yasnippet backend)))
;;       backend
;;     (append (if (consp backend) backend (list backend))
;;             '(:with company-yasnippet))))

;; (setq company-backends (mapcar #'company-backend-with-yas company-backends))


(provide 'eye-programming)
