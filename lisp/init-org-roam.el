
(add-to-list 'load-path "~/.emacs.d/packages/org-roam")
(add-to-list 'load-path "~/.emacs.d/packages/emacsql")
(add-to-list 'load-path "~/.emacs.d/packages/emacsql-sqlite3")
(require 'org-roam)
(setq org-roam-directory "~/org-roam-test")
(setq org-roam-tag-sources '(vanilla))
;;; Recommendation for Windows users for performance
;;; https://github.com/org-roam/org-roam/issues/1289#issuecomment-744046148
(setq org-roam-db-update-method 'immediate)

;;; Define key bindings for Org-roam
(global-set-key (kbd "C-c n r") #'org-roam-buffer-toggle-display)
(global-set-key (kbd "C-c n i") #'org-roam-insert)
(global-set-key (kbd "C-c n /") #'org-roam-find-file)
(global-set-key (kbd "C-c n b") #'org-roam-switch-to-buffer)
(global-set-key (kbd "C-c n d") #'org-roam-find-directory)

(setq org-roam-db-location "~/.emacs.d/org_roam_test.db")

(provide 'init-org-roam)
