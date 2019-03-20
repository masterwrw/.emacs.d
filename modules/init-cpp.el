(require 'cc-mode)

(add-hook 'c++-mode-hook 'yas-minor-mode)
(add-hook 'c-mode-hook 'yas-minor-mode)

(define-key c++-mode-map (kbd "<M-up>") 'beginning-of-defun)
(define-key c++-mode-map (kbd "<M-down>") 'end-of-defun)


(defun set-tab-width-hook ()
  (setq indent-tabs-mode nil)
  (setq default-tab-width 4)
  (setq tab-width 4)
  (setq c-basic-offset 4) ;; tab 缩进量
  (setq c-default-style "k&r") ;; 大括号缩进位置，https://en.wikipedia.org/wiki/Indentation_style
  (setq tab-stop-list ()))
(add-hook 'c-mode-hook 'set-tab-width-hook)
(add-hook 'c++-mode-hook 'set-tab-width-hook)

(defun eye/find-corresponding-file ()
  "Find the file that corresponds to this one."
  (interactive)
  (setq CorrespondingFileName nil)
  (setq BaseFileName (file-name-sans-extension buffer-file-name))
  (if (string-match "\\.c" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".h")))
  (if (string-match "\\.h" buffer-file-name)
      (if (file-exists-p (concat BaseFileName ".c")) (setq CorrespondingFileName (concat BaseFileName ".c"))
        (setq CorrespondingFileName (concat BaseFileName ".cpp"))))
  (if (string-match "\\.hin" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".cin")))
  (if (string-match "\\.cin" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".hin")))
  (if (string-match "\\.cpp" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".h")))
  (if (string-match "\\.c" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".h")))
  (if CorrespondingFileName (find-file CorrespondingFileName)
    (error "Unable to find a corresponding file")))

(add-hook 'c++-mode-common-hook
          '(lambda ()
             (local-set-key (kbd "C-c f") 'eye/find-correspoinding-file)))

(add-hook 'c-mode-common-hook
          '(lambda ()
             (local-set-key (kbd "C-c f") 'eye/find-correspoinding-file)))


(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))


;; 奇怪问题：在 emacs 中使用 mingw32-make 编译时总是报错无法找到引用，链接出错。
;; 但是在命令行下却又能成功编译。
;; 所以不直接调用 mingw32-make，而是调用 build.bat 批处理文件来进行编译。
(defvar build-script nil)
(if (eq system-type 'windows-nt)
    (setq build-script "build.bat")
  (setq build-script "build.sh")
  )

(setq qt-dir "C:\\Qt\\Qt4.8.7\\bin")
(setq qtcreator-dir "C:\\Qt\\qtcreator-4.6.0\\bin")
(setq gcc-dir "C:\\Qt\\Qt4.8.7\\bin")
(setq vs-env "C:\\Program Files (x86)\\Microsoft Visual Studio 12.0\\VC\\vcvarsall.bat")

(defun eye/set-gcc-env ()
  (let (path)
    (setq path (concat "@echo off\r\n"
                       "set path=%path%;" qt-dir ";" gcc-dir ";" qtcreator-dir ";" "\r\n"))
    path))

(defun eye/set-vs-env ()
  (let (path)
    (setq path (concat "@echo off\r\n"
                       "call \"" vs-env "\"" "\r\n"))
    path))

(defun eye/get-directory ()
  (let ((dir (read-directory-name "Project Directory: ")))
    (if (not (file-exists-p dir))
        (mkdir dir))
    dir))

(defun eye/create-qt-gcc-build-script ()
  (interactive)
  (let (dir file script command)
    (setq dir (eye/get-directory))
    (setq file (concat dir build-script))
    (setq command (format "mingw32-make -w -f Makefile.Release -C %s" dir))
    (setq script (concat (eye/set-gcc-env) command))
    (f-write script 'gbk file)
    ))

(defun eye/create-qt-vs-build-script ()
  (interactive)
  (let (dir file script command projectfile)
    (setq projectfile (read-file-name "Project file:"))
    (setq dir (file-name-directory projectfile))
    (setq file (concat dir build-script))
    (setq command (format "devenv \"%s\" /build" projectfile))
    (setq script (concat (eye/set-vs-env) command))
    (f-write script 'gbk file)
    ))

(require 'compile)
(setq compilation-directory-locked nil)

;; Compilation
(setq compilation-context-lines 0)
(setq compilation-error-regexp-alist
      (cons '("^\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:fatal error\\|warnin\\(g\\)\\) C[0-9]+:" 2 3 nil (4))
            compilation-error-regexp-alist))

(defun find-project-directory-recursive (x)
  "Recursively search for a makefile."
  (interactive)
  (if (file-exists-p x) t
    (cd "../")
    (find-project-directory-recursive x)))

(defun lock-compilation-directory ()
  "The compilation process should NOT hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked t)
  (message "Compilation directory is locked."))

(defun unlock-compilation-directory ()
  "The compilation process SHOULD hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked nil)
  (message "Compilation directory is roaming."))


(defun find-project-directory ()
  "Find the project directory."
  (interactive)
  (setq find-project-from-directory default-directory)
  ;;(switch-to-buffer-other-window "*compilation*")
  (if compilation-directory-locked (cd last-compilation-directory)
    (cd find-project-from-directory)
    (find-project-directory-recursive build-script)
    (setq last-compilation-directory default-directory)))


;; 在当前和上级目录中查找 Makefile 文件路径
(require 'cl) ; If you don't have it already
(defun* get-closest-pathname (&optional (file "Makefile"))
  "Determine the pathname of the first instance of FILE starting from the current directory towards root.
    This may not do the correct thing in presence of links. If it does not find FILE, then it shall return the name
    of FILE in the current directory, suitable for creation"
  (let ((root (expand-file-name "/"))) ; the win32 builds should translate this correctly
    (expand-file-name file
                      (loop
                       for d = default-directory then (expand-file-name ".." d)
                       if (file-exists-p (expand-file-name file d))
                       return d
                       if (equal d root)
                       return nil))))

;; For M-x compile
(defun build-command ()
  (set (make-local-variable 'compile-command)
       (get-closest-pathname build-script)))

(add-hook 'c++-mode-hook 'build-command)

(defun eye/compile-cpp ()
  (interactive)
  (let (command (get-closest-pathname build-script))
    (compile command))
  )



;; Success or failure of compile
(defun notify-compilation-result(buffer msg)
  "Notify that the compilation is finished."
  (if (string-match "^finished" msg)
      (progn
        ;;    (delete-windows-on buffer) ; Auto close compilation buffer
        (tooltip-show "\n Compilation Successful :-) \n "))
    (tooltip-show "\n Compilation Failed :-( \n ")))

(add-to-list 'compilation-finish-functions 'notify-compilation-result)


(defun make-without-asking ()
  "Make the current build."
  (interactive)
  (if (find-project-directory) (compile (concat "build.bat " (buffer-name (current-buffer)) )))
  ;;(switch-to-buffer-other-window "*compilation*")
  (delete-other-window)
  (switch-to-buffer "*compilation*"))

(defun real-make-without-asking ()
  "Make the current build."
  (interactive)
  (if (find-project-directory) (compile "make" ))
  (switch-to-buffer-other-window "*compilation*")
  (other-window 1))

(define-key c++-mode-map (kbd "<f5>") 'make-without-asking)

(require 'smart-compile)
(setq smart-compile-option-string "-w -s -j4")

(defun eye/cpp-help ()
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


;;;; auto insert
(require 'autoinsert)
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

;; (require 'srefactor)
;; (require 'srefactor-lisp)
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


(provide 'init-cpp)
