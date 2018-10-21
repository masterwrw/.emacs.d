;; ox-hugo require org version 9

(require 'ox)
(require 'ox-hugo)

(setq org-hugo-default-section-directory "post")
(setq org-hugo-front-matter-format "toml")
(setq org-hugo-auto-set-lastmod t)
(setq org-hugo-date-format "%Y-%m-%dT%T")

(defun eye/hugo-export-directory ()
  "导出一个org文件目录到hugo blog"
  (interactive)
  (let ((dir (read-directory-name "Select dir:")))
    (mapc (lambda (file-name)
            (if (not (file-directory-p (concat dir file-name)))
                (find-file (concat dir file-name)
                                  (org-hugo-export-to-md))))
          (directory-files dir nil "\.org$" t))
    ))



(provide 'ox-hugo-init)
