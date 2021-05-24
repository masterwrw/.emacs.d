;;; init-ctags.el --- ctags config for TAGS file     -*- lexical-binding: t; -*-

;; Copyright (C) 2019  

;; Author:  <soeye@SOEYE-WIN>
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




(provide 'init-ctags)
;;; init-ctags.el ends here
