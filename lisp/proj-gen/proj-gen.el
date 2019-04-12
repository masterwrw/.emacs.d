;;; proj-gen.el --- Project generator                -*- lexical-binding: t; -*-

;; Copyright (C) 2019  owensys

;; Author: owensys <owensys at hotmail dot com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'ido)

(defvar proj-gen-template-dir nil
  "模板文件目录")

(defvar proj-gen-file-encoding 'utf-8
  "默认输出的文件编码")

(defvar proj-gen-init-git nil
  "存在git的情况下是否自动初始化仓库")

(defun proj-gen-qt-project (dir name type)
  (let* ((template-dir (expand-file-name (format "%s" type) proj-gen-template-dir))
	 (proj-dir (expand-file-name name dir))
	 (pro-file-path (expand-file-name "proj.pro" proj-dir)))
    (if (file-directory-p template-dir)
	(copy-directory template-dir proj-dir)
      (message "template directory not exists."))
    ;; write .pro
    (if (file-exists-p pro-file-path)
	(progn (with-temp-buffer
		 (insert-file-contents pro-file-path)
		 (replace-string "%s" name nil (point-min) (point-max))
		 (set-buffer-file-coding-system proj-gen-file-encoding t nil)
		 (write-file pro-file-path)
		 (rename-file pro-file-path (expand-file-name (concat name ".pro") proj-dir))
		 )
	       (message "Generated finished.")
	       
	       ;; git init and commit
	       (when (and proj-gen-init-git (executable-find "git"))
		 (proj-gen-git-init proj-dir))
	       )
      (message "copy failed"))))


(defun proj-gen-git-init (path)
  "进入目录并初始化仓库，提交"
  (interactive "D")
  (let* ((default-directory path)
	 (git-buffer "*git*")
	 (proc (start-process "git" git-buffer shell-file-name shell-command-switch
			      (format "git init && git add . && git commit -m \"init repos\""))))
    (set-process-sentinel proc `(lambda (proc msg)
				  (let ((status (process-status proc)))
				    ;; (when (memq status '(exit signal))
				      (message "git commit:%s" msg)
				      )))
    (pop-to-buffer git-buffer)
    (read-only-mode 1)
    (goto-char (point-min))
    ))


(defun proj-gen-win32-project (dir name type)
  (message "generate win32 project"))

(defun proj-gen-cpp-project (type)
  "Create a c++ application project."
  ;; (interactive "GSelect project directory:")
  (let ((dir (ido-read-directory-name "Select project dir:"))
	(name (read-string "Project name:")))
    (when (and dir name)
      (or (file-exists-p dir) (make-directory dir))
      (cond ((string-match-p "qt" type)
	     (proj-gen-qt-project dir name type))
	    ((string-match-p "win32" type)
	     (proj-gen-win32-project dir name type))
	    (t (message "not support this type")))
    )))



;;;###autoload
(defun proj-gen-project ()
  (interactive)
  (let* ((options '("qt4-console" "qt4-gui" "qt5-console" "qt5-gui"
		    "win32-console" "win32-gui" "win32-lib" "win32-dll" "win32-duilib" "win32-mfc"))
	 (choice (ido-completing-read "Select:" options)))
    (when choice
      (proj-gen-cpp-project choice))))


;; (let ((proj-type-alist '(("qt4-console" . 'qt4-console)
;; ("qt4-gui" . 'qt4-gui)
;; ("qt5-console" . 'qt5-console)))
;; options choice type)
;; (mapcar (lambda (pair)
;; (push (car pair) options))
;; proj-type-alist)
;; (setq choice (ido-completing-read "Project type:" options))
;; (when choice
;; (setq type (cdr (assoc choice proj-type-alist)))
;; (message (format "%s select type: %s" choice type)))));;type > "quot ..."



(provide 'proj-gen)
;;; proj-gen.el ends here
