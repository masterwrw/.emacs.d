(require-maybe 'cc-mode)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; outline fold
(add-hook 'c++-mode-hook
	  (lambda ()
	    (outline-minor-mode 1)
	    (setq outline-regexp "^class\\|^struct\\|^enum\\|^[a-zA-Z][a-zA-Z0-9 _&\*]+::")))

;; show current function name
;; (if (fboundp 'set-header-line)
    ;; (add-hook 'c++-mode-hook 'set-header-line))

;; (add-hook 'c++-mode-hook 'yas-minor-mode)
;; (add-hook 'c-mode-hook 'yas-minor-mode)


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

(require-maybe 'compile)
(with-eval-after-load 'compile
  (setq compilation-directory-locked nil)
  ;; Compilation
  (setq compilation-context-lines 0)  
  
  (setq compilation-scroll-output t) ;;自动滚动
  (setq compilation-auto-jump-to-first-error t) ;;编译时有错误，则自动跳转到第一个错误
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
    (if (string-match " 0 个错误" (buffer-substring-no-properties (point-min) (point-max)))
	(progn
	  (tooltip-show "\n Compile Success \n ")
	  ;;自动关闭buffer @see https://emacs.stackexchange.com/questions/62/hide-compilation-window
          (run-at-time
           "2 sec" nil 'delete-windows-on
           (get-buffer-create "*compilation*"))
          (message "No Compilation Errors!"))
      (progn
	(tooltip-show "\n Compile Failed!!! :-( \n ")
	(message "Compilation Failed!!! :-(")))))
  
(add-to-list 'compilation-finish-functions 'notify-compilation-result)


;; For M-x compile
(defun build-command ()
  (let ((dir (locate-dominating-file buffer-file-name build-script)))
    (when dir
      (set (make-local-variable 'compile-command)
	   (expand-file-name build-script dir)))))

(add-hook 'c++-mode-hook 'build-command)

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


(defun eye/shell-cmake ()
  (interactive)
  (eye/shell-cmd "shell-cmake" (concat "C:\\green-soft\\git\\bin;"
                                       "C:\\green-soft\\cmake-3.11.0-rc4-win64-x64\\bin;"
                                       )))


(defun eye/create-class ()
  "Create a class based qt"
  (interactive)
  (let (class base-class filename)
    (setq class (read-string "Class name: "))  ;; input class name
    (setq base-class (read-string "Based: " "QWidget"))  ;; input base class
    (insert
     (with-temp-buffer
       (if (string-empty-p base-class)
           (insert-file-contents (expand-file-name (concat user-emacs-directory "template/cpp/class.h")))
       (insert-file-contents (expand-file-name (concat user-emacs-directory "template/cpp/class-qt.h"))))
       (beginning-of-buffer)
       (replace-string "name" class)
       (beginning-of-buffer)
       (replace-string "base" base-class)
       (buffer-string)
       ))
    (setq filename (file-name-nondirectory (buffer-file-name)))
    (with-temp-buffer
      (insert
       (with-temp-buffer
         (if (string-empty-p base-class)
             (insert-file-contents (expand-file-name (concat user-emacs-directory "template/cpp/class.h")))
           (insert-file-contents (expand-file-name (concat user-emacs-directory "template/cpp/class-qt.cpp"))))
         (beginning-of-buffer)
         (replace-string "name" class)
         (beginning-of-buffer)
         (replace-string "base" base-class)
         (beginning-of-buffer)
         (replace-string "header" (file-name-sans-extension filename))
         (buffer-string)
         ))
      (write-file (concat (file-name-sans-extension filename) ".cpp")))
    ))

;; Auto generate c++ class implement, function implement, functipn prototype
;;
;; 1.Generate class implement:
;; Move cursor to class name, call srefactor-refactor-at-point,
;; if selecte Other file and cpp file already has content, must open it first,
;; otherwise will be overwritten cpp file.
;;
;; 2.Generate function implement:
;; Move cursor to function name, call srefactor-refactor-at-point, Generate Function Implement
;;
;; 3.Generate function prototype:
;; Move cursor in function, call srefactor-refactor-at-point, Generate Function Prototype
;;
;; 4.Convert function to function pointer
;; Move cursor to function name, call srefactor-refactor-at-point, Generate Function Pointer
;;
;; 5.Extract region to a function:
;; Select a region, call srefactor-refactor-at-point.
;;
;; 6.Rename local variable name:
;; Move cursor on variable name, call srefactor-refactor-at-point
;;

;; (require-maybe 'srefactor)
;; (require-maybe 'srefactor-lisp)
;; (setq srefactor-ui-menu-show-help nil)

;; (setq semantic-idle-scheduler-idle-time 3)

;; maybe set semanticdb-find-default-throttle, https://emacs-china.org/t/topic/5728/6

;; (add-hook 'c++-mode-hook
                  ;; (lambda ()
                    ;; (semantic-mode 1)
                    ;; (semantic-idle-scheduler-mode 1)
                    ;; (remove-hook 'completion-at-point-functions 'semantic-analyze-completion-at-point-function)
                    ;; (remove-hook 'completion-at-point-functions 'semantic-analyze-notc-completion-at-point-function)
                    ;; (remove-hook 'completion-at-point-functions 'semantic-analyze-nolongprefix-completion-at-point-function)))

;; (define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
;; (define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)

(require-maybe 'autoinsert)
(defun eye-setup-c++ ()
  (define-auto-insert '(c++-mode . "C++ skeleton")
    '(
      (upcase (concat "_"
                      (replace-regexp-in-string
                       "[^A-Za-z0-9]" "_"
                       (file-name-nondirectory buffer-file-name))))
      "/*******************************************************************************" \n
      "Copyright: WRW.Tec" \n
      "Author: WRW" \n
      "Description: " \n
      "*******************************************************************************/" \n
      "#ifndef " str \n "#define " str "\n\n\n"
      "#endif"
      ))

  (define-key c++-mode-map (kbd "<M-up>") 'beginning-of-defun)
  (define-key c++-mode-map (kbd "<M-down>") 'end-of-defun)
  (define-key c++-mode-map (kbd "<f5>") 'make-without-asking)

  (defun set-tab-width-hook ()
    (setq indent-tabs-mode nil)
    (setq default-tab-width 4)
    (setq tab-width 4)
    (setq c-basic-offset 4) ;; tab 缩进量
    (setq c-default-style "k&r") ;; 大括号缩进位置，https://en.wikipedia.org/wiki/Indentation_style
    (setq tab-stop-list ()))
  (add-hook 'c-mode-hook 'set-tab-width-hook)
  (add-hook 'c++-mode-hook 'set-tab-width-hook)

  )


(with-eval-after-load 'cc-mode
  (eye-setup-c++))


;;;; company for cpp
(require-maybe 'company-c-headers)
(with-eval-after-load 'company-c-headers
  (add-hook 'c++-mode-hook (lambda () (add-to-list 'company-backends 'company-c-headers)))
  (when is-windows
    (setq company-c-headers-path-system '("C:/Program Files (x86)/Microsoft Visual Studio 14.0/VC/include"
					  "C:/Program Files (x86)/Microsoft SDKs/Windows\v7.1A/Include")))
  )





(provide 'init-cpp)
