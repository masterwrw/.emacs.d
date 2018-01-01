;; On-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


(defun reload-init ()
  "Reload the init file"
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(provide 'init-common)
