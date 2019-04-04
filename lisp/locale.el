(setq locale-notebook-dir "~/notebook/org/note")
(setq locale-notebook-attachment-dir "~/notebook/attach")
(setq locale-gtd-dir "~/notebook/org/gtd")
(setq locale-password-file "~/notebook/org/password.org")
(setq locale-custom-projects (list (concat locale-gtd-dir "/project/ud.org")
				   (concat locale-gtd-dir "/project/supertool.org")
				   ))
(setq locale-docset-dir "~/software/zeal-portable-0.5.0-windows-x64/docsets")


(defun eye/open-goals-file ()
  (interactive)
  (find-file-existing (concat locale-gtd-dir "/goals.org")))

;;(add-hook 'after-init-hook 'eye/open-goals-file)
