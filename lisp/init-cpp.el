(require 'cc-mode)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.hh\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))


(defun eye/find-header-or-source-file (&optional is-open-other-window)
  "Find the header or source file of this one."
  (interactive "P")
  (let ((full-base-name (file-name-sans-extension buffer-file-name)) ;;无后缀的文件路径
	(header-or-source-path nil))
    
    (cond ((string-match "\\.h" buffer-file-name)
	   (if (file-exists-p (concat full-base-name ".c"))
	       (setq header-or-source-path (concat full-base-name ".c"))
	     (setq header-or-source-path (concat full-base-name ".cpp"))))
	  
	  ((string-match "\\.c" buffer-file-name)
	   (setq header-or-source-path (concat full-base-name ".h")))
	  
	  ((string-match "\\.cpp" buffer-file-name)
	   (setq header-or-source-path (concat full-base-name ".h")))

	  (t (message "File name no suffix")))
    (if header-or-source-path
	(if is-open-other-window (find-file-other-window header-or-source-path)
	  (find-file header-or-source-path))
      (error "Unable to find a header or source file"))))


;; 奇怪问题：在 emacs 中使用 mingw32-make 编译时总是报错无法找到引用，链接出错。
;; 但是在命令行下却又能成功编译。
;; 所以不直接调用 mingw32-make，而是调用 build.bat 批处理文件来进行编译。
(defvar build-script (if is-windows "build.bat" "build.sh")
  "build script file name")

(require 'compile)
(with-eval-after-load 'compile
  (setq compilation-directory-locked nil)
  ;; Compilation
  (setq compilation-context-lines 0)  
  
  (setq compilation-scroll-output t) ;;自动滚动
  ;; 编译时有错误，则自动跳转到第一个错误，设置为nil，避免rg搜索出现报错
  (setq compilation-auto-jump-to-first-error nil)
  (setq compilation-always-kill t) ;;执行编译时，如果有前一个编译命令正在执行，自动kill，不询问
  ;; 使next-error跳过warning @see https://emacs-china.org/t/compilation-mode-next-error-warning/9095/10
  ;; 或者使用pcre2el包使用pcre的正则语法来匹配错误
  (setq compilation-skip-threshold 2)
  (if (bound-and-true-p rxt-pcre-to-elisp) ;; pcre2el,https://github.com/joddie/pcre2el
      (progn
	(add-to-list 'compilation-error-regexp-alist 'fixed-msvc)
	(add-to-list 'compilation-error-regexp-alist-alist
		     `(fixed-msvc
		       ,(rxt-pcre-to-elisp (concat
					    "^\\s*(?:\\d+>\\s*)?"  ; for msbuild, it will add "\d+>" on each line
					    "("                    ; group 1: hyperlink
					    "((?:\\w:)?[^:\t\n]+?)" ; group 2: file path
					    "(?:\\((\\d+)\\))?"    ; group 3: line number
					    "\\s*:\\s*"
					    "(?:(note)|(warning)|(fatal )?error)(\\s+C\\d+)?" ; group 4: note, group 5: warning
					    "\\s*:"
					    ")"))
		       2 3 nil (5 . 4) 1))
	)
    ;; emacs本身的正则语法
    (setq compilation-error-regexp-alist
	  (cons '("^\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:fatal error\\|warnin\\(g\\)\\) C[0-9]+:" 2 3 nil (4))
		compilation-error-regexp-alist)))


  )


;; Success or failure of compile
(defun notify-compilation-result(buffer msg)
  "Notify that the compilation is finished.
通过build.bat中msbuild编译，有错误，但msg总是为\"finished\"
"
  (with-current-buffer buffer
    ;;0前面增加一个空格，避免匹配到10,20等
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (if (or (string-match " 0 个错误" content)
	      (string-match "^rg finished" content)
	      (string-match "color-rg finished" content))
	(progn
	  (tooltip-show "\n Compile Success \n ")
	  ;;自动关闭buffer @see https://emacs.stackexchange.com/questions/62/hide-compilation-window
          (run-at-time
           "2 sec" nil 'delete-windows-on
           (get-buffer-create "*compilation*"))
          (message "No Compilation Errors!"))
      (progn
	(tooltip-show "\n Compile Failed!!! :-( \n ")
	(message "Compilation Failed!!! :-("))))
    ))
  
(add-to-list 'compilation-finish-functions 'notify-compilation-result)


;; For M-x compile

;; 必须判断一下buffer-file-name是否是nil，否则出现奇怪问题，org导出html时，也会调用c++-mode-hook，但是这个为nil，导致导出不了，并卡顿，错误如下：
;; Debugger entered--Lisp error: (wrong-type-argument stringp nil)
;;   expand-file-name(nil)
;;   locate-dominating-file(nil "build.bat")
;;   (let ((dir (locate-dominating-file buffer-file-name build-script))) (if dir (progn (set (make-local-variable 'compile-command) (expand-file-name build-script dir)))))
;;   build-command()
;;   run-hooks(change-major-mode-after-body-hook prog-mode-hook c-mode-common-hook c++-mode-hook)
;;   apply(run-hooks (change-major-mode-after-body-hook prog-mode-hook c-mode-common-hook c++-mode-hook))
;;   run-mode-hooks(c++-mode-hook)
;;   c++-mode()
;;   org-html-fontify-code("" "C++")
(defun build-command ()
  (when buffer-file-name
    (let ((dir (locate-dominating-file buffer-file-name build-script)))
      (when dir
	(set (make-local-variable 'compile-command)
	     (expand-file-name build-script dir))))))


(defun eye/auto-compile ()
  (interactive)
  (when compile-command
    (compile compile-command)))

(defun eye/search-cpp-doc ()
  "Find cpp reference document."
  (interactive)
  (let ((url "http://zh.cppreference.com/mwiki/index.php?search="))
    (setq url (concat url (read-string "Query cpp document: " (eye/current-word))))
    (browse-url-firefox url)))

(defun eye-setup-c++ ()
  (setq-default tab-width 4)
  (setq tab-width 4)
  (setq-default c-default-style "k&r")
  ;; outline fold
  (outline-minor-mode 1)
  (setq outline-regexp "^class\\|^struct\\|^enum\\|^[a-zA-Z][a-zA-Z0-9 _&\*]+::")
  (build-command)
  (yas-minor-mode 1)
  (define-key c++-mode-map (kbd "<M-up>") 'beginning-of-defun)
  (define-key c++-mode-map (kbd "<M-down>") 'end-of-defun)
  (define-key c++-mode-map (kbd "<f5>") 'make-without-asking)
  )


(add-hook 'c++-mode-hook #'eye-setup-c++)
(add-hook 'c-mode-hook #'eye-setup-c++)

;; (add-hook 'c++-mode-hook ')
;; (add-hook 'c-mode-hook 'yas-minor-mode)



(provide 'init-cpp)
