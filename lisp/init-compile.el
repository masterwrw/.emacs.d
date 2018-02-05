;;; Qt mode, compile configuration
(require 'compile)

(setq compilation-directory-locked nil)

;; Other packages
(setq owen-font "outline-DejaVu Sans Mono")

(when *is-windows*
  (setq owen-makescript "build.bat")
  )

(when *is-linux*
  (setq owen-makescript "build.linux")
  )


(defun owen-big-fun-compilation-hook ()
  (make-local-variable 'truncate-lines)
  (setq truncate-lines nil)
  )
(add-hook 'compilation-mode-hook 'owen-big-fun-compilation-hook)


;; Compilation
(setq compilation-context-lines 0)
(setq compilation-error-regexp-alist
      (cons '("^\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:fatal error\\|warnin\\(g\\)\\) C[0-9]+:" 2 3 nil (4))
	    compilation-error-regexp-alist))

(defun find-project-directory-recursive (x)
  "Recursively search for a makefile."
  (interactive)
  (if (file-exists-p x) t
    (cd "../")
    (find-project-directory-recursive x)))

(defun lock-compilation-directory ()
  "The compilation process should NOT hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked t)
  (message "Compilation directory is locked."))

(defun unlock-compilation-directory ()
  "The compilation process SHOULD hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked nil)
  (message "Compilation directory is roaming."))

(defun makescript (x)
  ("build.bat"))

(defun find-project-directory ()
  "Find the project directory."
  (interactive)
  (setq find-project-from-directory default-directory)
					;(switch-to-buffer-other-window "*compilation*")
  (if compilation-directory-locked (cd last-compilation-directory)
    (cd find-project-from-directory)
    (find-project-directory-recursive "Makefile")
    (setq last-compilation-directory default-directory)))

(defun make-without-asking ()
  "Make the current build."
  (interactive)
  (if (find-project-directory) (compile (concat "build.bat " (buffer-name (current-buffer)) )))
  (switch-to-buffer-other-window "*compilation*")
  (other-window 1))
(define-key global-map "\em" 'make-without-asking)

(defun real-make-without-asking ()
  "Make the current build."
  (interactive)
  (if (find-project-directory) (compile "make" ))
  (switch-to-buffer-other-window "*compilation*")
  (other-window 1))
					;(define-key global-map "\em" 'real-make-without-asking)


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
(setq smart-compile-option-string "-w -s -j4")



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
