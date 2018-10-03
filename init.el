;;; 设置 require 路径
(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))

(add-subdirs-to-load-path (concat user-emacs-directory "config/"))
(add-subdirs-to-load-path (concat user-emacs-directory "themes/"))
(add-subdirs-to-load-path (concat user-emacs-directory "site-lisp/"))

(require 'all-init)
