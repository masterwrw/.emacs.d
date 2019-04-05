
(defun eye/open-bash ()
  (interactive)
  (async-shell-command "C:\\software\\PortableGit\\git-bash.exe")  
  ;;if use async-shell-command, can't kill that buffer
  ;; (kill-buffer "*Shell Command Output*")
  (delete-other-windows))



(provide 'init-external)
