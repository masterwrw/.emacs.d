(require-package 'find-file-in-project)
(require-package 'magit)


;; projectile
(require-package 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching 1)
(diminish 'projectile-mode "prj")



(provide 'init-project-manager)
