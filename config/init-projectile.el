(use-package ag :ensure t) ;; for projectile ag search

(use-package projectile
  :ensure t
  :bind ("<f3>" . 'projectile-find-file)
  :init
  (setq projectile-enable-caching 1)
  (setq projectile-globally-ignored-file-suffixes '(".sdf" ".opensdf" ".ffs_db" ".user" ".obj" "*.suo" "*.dll" ".pdb"))
  (setq projectile-globally-ignored-directories '(".vs" ".git" "ipch" "tmp"))
  (projectile-mode 1))



;;; search and grep
(use-package swiper
  :ensure t
  :bind
  ("C-f"   . 'swiper))

(defun eye/grep ()
  (interactive)
  (let* ((cur-word (thing-at-point 'word))
         (cmd (concat "grep --color -irHn " cur-word " " (buffer-file-name))))
    (setq cmd (read-from-minibuffer "command:" cmd))
    (grep-apply-setting 'grep-command cmd)
    (grep cmd)))

(use-package wgrep :ensure t)
(use-package wgrep-ag :ensure t)


(use-package counsel
  :ensure t
  :config
  (let ((command
	 (cond
          ((executable-find "rg")
           "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
          ((executable-find "ag")
           "ag -i --noheading --nocolor --nofilename --numbers '%s' %s"))))
    (setq counsel-grep-base-command command))

  (global-set-key (kbd "M-f")
		  (lambda ()
		    (interactive)
		    (counsel-M-x "^counsel ")))
  )


;;(use-package helm-config
;;  :ensure t
;;  :config  
;;  (define-key helm-command-map (kbd "M-o") 'helm-occur)
;;  (define-key helm-command-prefix (kbd "M-o") 'helm-occur))


(use-package ivy
  :ensure t
  :config
  (define-key ivy-minibuffer-map (kbd "M-o") 'ivy-occur))


(provide 'init-projectile)
