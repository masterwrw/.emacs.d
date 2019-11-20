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

(if is-linux
    (progn
      (setq org-note-files-dir "/mnt/windows/note/wikinote")
      (setq org-note-html-dir "/mnt/windows/note/html")
      (setq org-note-attach-dir "/mnt/windows/note/a"))
  (progn
    (setq org-note-files-dir "f:/wikinote")
    (setq org-note-html-dir "f:/html")
    (setq org-note-attach-dir "f:/a")))
    


(defun org-note-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
  "org-html-export-to-html默认导出在当前目录，修改为导出到指定目录，locale-notebook-dir的上级目录下html目录"
  (interactive)
  (let* ((extension (concat "." (or (plist-get ext-plist :html-extension)
				    org-html-extension
				    "html")))
	 (file (org-export-output-file-name extension subtreep org-note-html-dir))
	 (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'html file
      async subtreep visible-only body-only ext-plist)))


(defun org-note-open-file-html ()
  "使用默认浏览器打开当前文件的url
http://blog.binchen.org/posts/open-url-in-emacs-with-external-browser.html"
  (interactive)
  (let (url (path (expand-file-name (buffer-name) org-note-html-dir)))
    (setq url (concat "file:///" path))
    (setq url (replace-regexp-in-string ".org$" ".html" url))
    (browse-url-default-browser url)
    ))

(defun org-note-export-and-open ()
  (interactive)
  (org-note-export-to-html)
  (org-note-open-file-html))


;; 自动添加的文件头
(setq org-note-template
      (concat "#+TITLE: %n\n"
	      "#+KEYWORDS: \n"
	      "#+DATE: %d\n"
	      "#+INCLUDE: style.org\n"
	      "#+OPTIONS: ^:nil \\n:t\n"
	      "#+STARTUP: hideblocks\n"
	      "- [[file:index.org][Index]]\n\n"
	      "- Related: \n\n"
	      "* %n\n"))

(defun org-note-header ()
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
                 (file-name-base (buffer-file-name))
                 org-note-template))
         ;; Replace %d by current date in the format %Y-%m-%d
         (text2 (replace-regexp-in-string
                 "%d"
                 (format-time-string "%Y-%m-%d")
                 text1
                 )))
      ;; Got to top of file
      (goto-char (point-min))
      (insert text2))))



(defun org-note-new ()
  "Create a new wiki page and open it without inserting a link."
  (interactive)
  (let* ((pagename (read-string "Page Name: "))
	 (notefile (concat (file-name-as-directory org-note-files-dir)
			    pagename ".org")))
    (if (not (file-exists-p notefile))
	(progn
	  (find-file notefile)
	  (org-note-header)
	  (save-buffer)
	  (goto-char (point-max)))
      (find-file notefile))))


(defun org-note-search-keywords ()
  "Search keywords use counsel-ag or grep"
  (interactive)
  (if (and (fboundp 'counsel-ag) (executable-find "ag"))
      (let ((str (read-string "Search Keywords: "))
	    (default-directory org-note-files-dir))
	(counsel-ag (concat "^#\\+KEYWORDS " str)))
    
    (let ((str (read-string "Search Keywords: " ".*"))
	  (default-directory org-note-files-dir))
      (rgrep (concat "^#+KEYWORDS:" str) "*.org" org-note-files-dir nil))))



(defun org-note-search-title ()
  "Search title use counsel-ag or grep"
  (interactive)
  (if (and (fboundp 'counsel-ag) (executable-find "ag"))
      (let ((str (read-string "Search Title: "))
	    (default-directory org-note-files-dir))
	(counsel-ag (concat "^#\\+TITLE " str)))
    
    (let ((str (read-string "Search Title: " ".*"))
	  (default-directory org-note-files-dir))
      (rgrep (concat "^#+TITLE:" str) "*.org" org-note-files-dir nil))))
  


(provide 'init-org-note)
