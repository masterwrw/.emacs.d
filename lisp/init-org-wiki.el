(require 'org-wiki)
(setq org-wiki-location-list '("H:/wikinote"))
(setq org-wiki-location (car org-wiki-location-list))
;; (setq org-wiki-attach-directory "~/org/attach")
(setq org-wiki-clip-jar-path (expand-file-name "bin/Clip.jar" user-emacs-directory))

;; for export
(setq org-wiki-emacs-path "C:\\software\\Emacs26.1\\emacs-26.1-x86_64\\bin\\runemacs.exe")
(setq org-wiki-user-init-file (concat (file-name-as-directory user-emacs-directory) "lisp/wiki-export.el"))

;; (setq org-wiki-template
;;       (concat "#+TITLE: %n\n"
;; 	      "#+DESCRIPTION:\n"
;; 	      "#+KEYWORDS:\n"
;; 	      "#+STARTUP: content\n"
;; 	      "#+CREATED: %d\n\n\n"
;; 	      "- PT: \n\n"
;; 	      "- JT: \n\n"
;; 	      "* %n\n\n"
;; 	      "- CT: \n\n"
;; 	      "- AT: \n\n\n"))

(setq org-wiki-template
      (concat "#+TITLE: %n\n"
	      ;; "#+OPTIONS: ^:nil \\n:t\n\n"
	      ;; "#+AUTHOR: owensys\n"
	      ;; "#+KEYWORDS:\n"
	      ;; "#+STARTUP: content\n"
	      "#+DATE: %d\n"
	      "#+INCLUDE: style.org\n\n"
	      "- [[wiki:index][Index]]\n\n"
	      "- Related: \n\n"
	      "* %n\n"))

;; 导出html的样式
;; (setq org-html-head (concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"theme/org-nav/my-style.css\" />"
			    ;; "<script src=\"theme/org-nav/org-nav-theme.js\"></script>"))
(setq org-html-head nil)    ;使用#+INCLUDE



(defun org-wiki-from-url (url)
  "从浏览器复制url过来后，直接打开路径下的org文件"
  (interactive "sUrl:")
  (if (string-match "^file:///" url)
      (let ((tempurl url) last)
	(setq tempurl (substring url 8 (length url))) ;取后面的
	(setq last (string-match ".html" tempurl))
	(setq tempurl (substring tempurl 0 last))
	(setq tempurl (concat tempurl ".org"))
	(if (file-exists-p tempurl) 
	    (find-file tempurl)
	  (message "file not exists"))
	)
    (message "no scheme in url")))


(defun org-wiki-open-url ()
  "使用默认浏览器打开当前文件的url
http://blog.binchen.org/posts/open-url-in-emacs-with-external-browser.html"
  (interactive)
  (let (url (path (expand-file-name (buffer-file-name))))
    (setq url (concat "file:///" path))
    (setq url (replace-regexp-in-string ".org$" ".html" url))
    (browse-url-default-browser url)
    ))



;; 自定义一些导出过滤函数
;; (defun my-test-filter-func (text backend info)
       ;; (replace-regexp-in-string "_" "~" text))
;; (add-to-list 'org-export-filter-underline-functions
             ;; 'my-test-filter-func)






(provide 'init-org-wiki)
