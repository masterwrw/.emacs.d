;; first version
;; (defun eye/open-external ()
  ;; "Open locale program"
  ;; (interactive)
  ;; (let ((program (ido-completing-read "Run:" '("bash" "keepassx" "qtcreator"))))
    ;; (cond ((string-equal program "bash") (message "run bash"))
	  ;; ((string-equal program "keepassx") (message "run keepassx"))
	  ;; ((string-equal program "qtcreator") (message "run qtcreator"))
	  ;; (t (message "No choice")))))

;; second version
(setq eye-program-alist '(("bash" . "C:/software/PortableGit/git-bash.exe")
			  ("keepassx" . "C:/software/KeePassX-2.0.3/KeePassX.exe")
			  ("qtcreator" . "c:/Qt/Qt5.7.1/Tools/QtCreator/bin/qtcreator.exe")
			  ("file manager" . (lambda () (eye/open-file-manager)))
			  ("xfce terminal" . "xfce4-terminal")
			  ("calc" . "c:/Windows/System32/calc.exe")
			  ("github desktop" . "C:/Users/soeye/AppData/Local/GitHubDesktop/GitHubDesktop.exe")
			  ("cmake" . "C:/software/cmake-3.14.2-win32-x86/bin/cmake-gui.exe")
			  ))
(defun eye/open-external ()
  "Open locale program"
  (interactive)
  (let (strlist sel)
    (mapcar (lambda (elem)
	      (add-to-list 'strlist (car elem)))
	    eye-program-alist)
    (setq sel (ido-completing-read "Run:" strlist))
    (when sel
      (let ((value (cdr (assoc sel eye-program-alist))))
	(if (stringp value)
	    (progn
	      (if (executable-find value)
		  (async-shell-command value)
		(message "Can not find program: %s" value)))
	  (funcall value))))
    (delete-other-windows)))


(provide 'init-external)
