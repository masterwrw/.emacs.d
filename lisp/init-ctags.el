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

(defun eye/create-ctags-file ()
  "Create ctags file"
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
(append-to-list 'tags-table-list locale-system-tags-paths)
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

;; (eval-after-load 'cc-mode
;;   (defhydra hydra-ctags (:exit t)
;;   ("l" eye/load-project-root-tags "load root tags")))




(provide 'init-ctags)
;;; init-ctags.el ends here
