;;;;
;; 
;; 注意：如果路径使用了samba映射的，则需要设置debug-on-error为nil，否则保存文件时会报错set-file-acl...
;;

;; org文件主目录
(setq org-wiki-location "n:/org/note")
(setq org-wiki-exports-dir "n:/org/html")
(setq org-wiki-attach-dir "o:/org/attach")

;; 不在这里设置自定义head内容，而是使用#+INCLUDE
(setq org-html-head nil)
(setq org-export-with-smart-quotes t)
(setq org-ascii-links-to-notes nil)
(setq org-ascii-headline-spacing (quote (1 . 1)))
(setq org-html-coding-system 'utf-8-unix)
(setq org-html-validation-link nil) ;;Remove validation link
;;MathJax CDN
(setf org-html-mathjax-options
      '((path "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
        (scale "100") 
        (align "center") 
        (indent "2em")
        (mathml nil))
      )
(setf org-html-mathjax-template
      "<script type=\"text/javascript\" src=\"%PATH\"></script>")

;;Table
(setq org-html-table-default-attributes
      '(:border "0" :cellspacing "0" :cellpadding "6" :rules "none" :frame "none"))


(eval-after-load "org"
  '(require 'ox-publish nil t))




(setq org-publish-project-alist
      '(
        ;; First Project
        ("org-notes"
         :base-directory "N:/org/note"
         :base-extension "org"
         :publishing-directory "N:/org/html/"
         :recursive nil
         :exclude ".*-template\.org\\|README\.org"        ; exclude org-reveal slides and other files 
         :publishing-function org-html-publish-to-html
         :headline-levels 2                ; Just the default for this project.
;         :auto-sitemap t                  ; Generate sitemap.org automagically...
;         :sitemap-filename "org-sitemap.org"  ; ... call it sitemap.org (it's the default)...
;         :sitemap-title "Plan du site"         ; ... with title 'Sitemap'.
         :with-creator nil    ; Disable the inclusion of "Created by Org" in the postamble.
         :with-email nil      ; Disable the inclusion of "(your email)" in the postamble.
         :with-author nil       ; Enable the inclusion of "Author: Your Name" in the postamble.
         :auto-preamble t;         ; Enable auto preamble 
         :auto-postamble t         ; Enable auto postamble 
         :table-of-contents t        ; Set this to "t" if you want a table of contents, set to "nil" disables TOC.
         :toc-levels 1               ; Just the default for this project.
         :section-numbers nil        ; Set this to "t" if you want headings to have numbers.
         :html-head-include-default-style nil ;Enable the default css style
         :html-head-include-scripts nil ;Disable the default javascript snippet
;         :html-head "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n<link rel=\"stylesheet\" type=\"text/css\" href=\"css/org.css\"/>\n<script type=\"text/javascript\" src=\"js/ga.min.js\"></script>" ;Enable custom css style and other tags
;         :html-link-home "index.html"    ; Just the default for this project.
;         :html-link-up "misc.html"    ; Just the default for this project.
         )
	;;("org" :components ("org-notes"))
        ))



;; 自定义页面尾部的内容（无效）
;(setq org-html-postamble-format
;      '(("en" "<p class=\"postamble\">Last Updated %d. Created by %c</p>")))


(defun org-html-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
	
	;;(message "html code block, lang:%s, code:%s" (org-element-property :language src-block)
	;;		 (org-element-property :value src-block)
	;;		 )
	
    (let ((lang (org-element-property :language src-block))
          (code (org-element-property :value src-block)))
	  (format "<pre><code class=\"%s\">\n%s\n</code></pre>" (downcase lang) code)
      )))


(defun my-org-wiki-is-note-file ()
  (and (equal major-mode 'org-mode)
	   (string-suffix-p ".org" (buffer-file-name))
	   (string-match-p (expand-file-name org-wiki-location) (buffer-file-name))))

(defun my-org-wiki-auto-export-hook ()
  "自动导出html文件"
  (interactive)
  (when (my-org-wiki-is-note-file)
	(org-html-export-to-html)
	(when (not (file-exists-p org-wiki-exports-dir))
	  (mkdir org-wiki-exports-dir))

	;; 移动文件到html目录
	(let* ((basename (file-name-base (buffer-file-name)))
		   (oldpath (concat default-directory "/" basename ".html"))
		   (newpath (concat org-wiki-exports-dir "/" basename ".html")))
	  (copy-file oldpath newpath t)
	  (delete-file oldpath))
	
	(message "Export file:%s finished!" (buffer-file-name))
	))

;;(add-hook 'after-save-hook 'my-org-wiki-auto-export-hook)



(defun my-org-wiki-from-url (url)
  "从浏览器复制url过来后，直接打开路径下的org文件"
  (interactive "sUrl:")
  (when (string-match "^file:///" url)
      (let* ((basename (file-name-base url))
			 (orgfile (concat org-wiki-location "/" basename ".org")))
		(if (file-exists-p orgfile) 
			(find-file orgfile)
		  (message "file not exists:%s" orgfile)))
      ))



(defun my-org-wiki-open-url ()
  "使用默认浏览器打开当前文件的url
http://blog.binchen.org/posts/open-url-in-emacs-with-external-browser.html"
  (interactive)
  (when (my-org-wiki-is-note-file)
	(save-buffer)
	(let ((url (concat "file:///"
					   (expand-file-name org-wiki-exports-dir) "/"
					   (file-name-base (buffer-name))
					   ".html")))
      (browse-url-default-browser url))))


(defun my-org-wiki-save-and-open-url()
  (interactive)
  (my-org-wiki-auto-export-hook)
  (my-org-wiki-open-url))


(defun my-org-wiki-open-dir ()
  (interactive)
  (dired locale-notebook-dir))


(defun my-org-wiki-open-attach-dir ()
  (interactive)
  (if is-windows
	  (w32-shell-execute "open" "explorer" "N:\\org\\attach")))


(my-org-wiki-open-attach-dir)

(provide 'init-my-orgwiki)
