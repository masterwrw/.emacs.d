;;; Qt mode, compile configuration

(require 'cl) ; If you don't have it already
(defun* get-closest-pathname (&optional (file "Makefile"))
  "Determine the pathname of the first instance of FILE starting from the current directory towards root.
This may not do the correct thing in presence of links. If it does not find FILE, then it shall return the name
of FILE in the current directory, suitable for creation"
  (let ((root (expand-file-name "/"))) ; the win32 builds should translate this correctly
    (expand-file-name file
		      (loop
		       for d = default-directory then (expand-file-name ".." d)
		       if (file-exists-p (expand-file-name file d))
		       return d
		       if (equal d root)
		       return nil))))


(defun in-directory (dir)
  "Runs execute-extended-command with default-directory set to the given
directory."
  (interactive "DIn directory: ")
  (let ((default-directory dir))
    (call-interactively 'execute-extended-command)))



(require-package 'smart-compile)
(require 'smart-compile)
(setq smart-compile-option-string "-w -s -j4"))



; Success or failure of compile
(defun notify-compilation-result(buffer msg)
  "Notify that the compilation is finished."
  (if (string-match "^finished" msg)
      (progn
	(delete-windows-on buffer) ; Auto close compilation buffer
	(tooltip-show "\n Compilation Successful :-) \n "))
      (tooltip-show "\n Compilation Failed :-( \n ")))

(add-to-list 'compilation-finish-functions 'notify-compilation-result)



(defun recompile-quietly ()
  "Re-compile without changing the window configuration."
  (interactive)
  (save-window-excursion
    (recompile)))



;; For M-x compile
(defun mingw32-make-command ()
  (set (make-local-variable 'compile-command)
       (format "mingw32-make -w -s -j4 -f Makefile.Release -C %s"
	       (file-name-directory (get-closest-pathname "Makefile.Release")))))

(when *is-windows*
  (add-hook 'c++-mode-hook 'mingw32-make-command))




(provide 'init-compile)
