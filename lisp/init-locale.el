(eye--reset-time)

(setq locale-notebook-dir "~/notebook/org/note")
(setq locale-notebook-attachment-dir "~/notebook/attach")
(setq locale-gtd-dir "~/notebook/org/gtd")
(setq locale-password-file "~/notebook/org/password.org")
(setq locale-custom-projects (list (concat locale-gtd-dir "/project/ud.org")
				   (concat locale-gtd-dir "/project/supertool.org")
				   ))
(setq locale-docset-dir "~/software/zeal-portable-0.5.0-windows-x64/docsets")


(add-to-list 'exec-path "C:/software/Emacs26.1/ctags-2019-01-18_5a2b65f8-x64")
(add-to-list 'exec-path "C:/software/Emacs26.1/global663wb/bin")
(add-to-list 'exec-path "C:/software/Emacs26.1/Searcher")
(add-to-list 'exec-path "C:/software/PortableGit")


(defun eye/open-goals-file ()
  (interactive)
  (find-file-existing (concat locale-gtd-dir "/goals.org")))


(defun eye/open-locale-file ()
  (interactive)
  (find-file locale-config-file))





(eye--print-time "load locale")




(provide 'init-locale)
