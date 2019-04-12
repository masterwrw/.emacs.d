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
			  ("file manager" . (lambda () (eye/open-file-manager)))))
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
	    (async-shell-command value)
	  (funcall value))))
    (delete-other-windows)))

(funcall (function 'eye/open-file-manager))


(provide 'init-external)
