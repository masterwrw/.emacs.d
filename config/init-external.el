(defun eye/open-file-manager ()
  "Open external file manager."
  (interactive)
  (when (and (executable-find "thunar")
	     (not (null default-directory)))
    (start-process "File manager" nil "thunar" default-directory)))

(defun eye/open-terminal ()
  (interactive)
  (when (executable-find "xfce4-terminal")
    (start-process "Terminal" nil "xfce4-terminal")))


(provide 'init-external)
