;; init-org-note.el 管理org笔记，包括导出的html，附件
;; 目录结构
;; ~/note
;; ----org         所有org文件，子目录不需要包含org文件，如果有子目录，也只应该是临时的，需要整理
;; ----org/inbox   收集
;; ----a           所有附件
;; ----html        导出的html文件
;; ----html/theme  样式文件

(require 'ox-html)
(require 'htmlize)

(setq org-note-files-dir (if is-linux "/home/dev/orgnote" "~/orgnote"))
(setq org-note-attach-dir (if is-linux "/home/dev/orgnote/a" "~/orgnote/a"))

;; 自动添加的文件头
(setq org-note-template
      (concat "#+BEGIN_COMMENT\n"
	      ".. title: %n\n"
	      ".. slug: %g\n"
	      ".. date: %d UTC+08:00\n"
	      ".. tags: draft,\n"
	      ".. category: \n"
	      ".. link: \n"
	      ".. description: \n"
	      ".. type: text\n"
	      "#+END_COMMENT\n"
	      "#+INCLUDE: org-style.ss\n\n\n"
	      "* %n\n"
	      ))

(defun org-note-header (str-title uuid)
  "Insert a header at the top of the file.
This is copy from org-wiki package, https://github.com/caiorss/org-wiki
"
  (interactive)
  ;; Save current cursor location and restore it
  ;; after completion of block insider save-excursion.
  (save-excursion
    (let*
        ;; replace '%n' by page title
        ((text1 (replace-regexp-in-string
                 "%n"
                 str-title
                 org-note-template))
         ;; Replace %d by current date in the format %Y-%m-%d
         (text2 (replace-regexp-in-string
                 "%d"
                 (format-time-string "%Y-%m-%d %H:%M:%S")
                 text1
                 ))
	 (text3 (replace-regexp-in-string
                 "%g"
		 ;;(org2nikola-get-slug str-title) ;; 自动得到中文的拼音
		 (concat uuid "-" str-title)
                 text2
                 )))
      ;; Got to top of file
      (goto-char (point-min))
      (insert text3))))

(random t)
(defun get-note-name-uuid ()
  (format "%06x%06x" (random (expt 16 6))
  (random (expt 16 6))))


(defun org-new-page (note-dir)
  "Create a new wiki page and open it without inserting a link."
  (let* ((str-title (read-string "Title: "))
	 (uuid (get-note-name-uuid))
	 (notefile (concat (file-name-as-directory note-dir)
			   uuid "-" str-title ".org")))
    (if (not (file-exists-p notefile))
	(progn
	  (find-file notefile)
	  (org-note-header str-title uuid)
	  (save-buffer)
	  (goto-char (point-max)))
      (find-file notefile))))

(defun org-new-note ()
  (interactive)
  (org-new-page org-note-files-dir))

(defun org-new-post ()
  "Create a new wiki page and open it without inserting a link."
  (interactive)
  (org-new-page "/home/dev/orgsite/posts"))


(defun org-note-search-tag ()
  "Search tags use counsel-ag or grep"
  (interactive)
  (if (and (fboundp 'counsel-ag) (executable-find "ag"))
      (let ((str (read-string "Search Tag: "))
	    (default-directory org-note-files-dir))
	(counsel-ag (concat "^.. tags: " str)))
    (message "Please install ag for search quickly!")))


(defun org-note-search-title ()
  "Search title use counsel-ag or grep"
  (interactive)
  (if (and (fboundp 'counsel-ag) (executable-find "ag"))
      (let ((str (read-string "Search Title: "))
	    (default-directory org-note-files-dir))
	(counsel-ag (concat "^.. title: " str)))
    (message "Please install ag for search quickly!")))


(provide 'init-org-note)
