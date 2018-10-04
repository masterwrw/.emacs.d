;; http://ergoemacs.org/emacs/elisp_traverse_dir.html
;; (defun git-submodule-add ()
  ;; (interactive)
  ;; (let ((defualt-dir default-directory))
    ;; (setq default-directory user-emacs-directory)
    ;; (async-shell-command "git submodule add https://github.com/bbatsov/projectile site-lisp/projectile")))


(defun compile-package ()
  (interactive)
  (let ((dir (read-directory-name "Package dir:" "~/emacs-config/site-lisp")))
    (mapc (lambda (x) (byte-compile-file x))
	  (directory-files-recursively dir "\.el$" ))))


