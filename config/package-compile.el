;; http://ergoemacs.org/emacs/elisp_traverse_dir.html
(defun compile-package ()
  (interactive)
  (let ((dir (read-directory-name "Package dir:" "~/emacs-config/site-lisp")))
    (mapc (lambda (x) (byte-compile-file x))
	  (directory-files-recursively dir "\.el$" ))))


