(require 'org-wiki)
(setq org-wiki-location-list '("~/org/wiki"))
(setq org-wiki-location (car org-wiki-location-list))
(setq org-wiki-attach-directory "~/org/attach")
(setq org-wiki-clip-jar-path (expand-file-name "bin/Clip.jar" user-emacs-directory))

(setq org-wiki-template
      (concat "#+TITLE: %n\n"
	      "#+DESCRIPTION:\n"
	      "#+KEYWORDS:\n"
	      "#+STARTUP: content\n"
	      "#+CREATED: %d\n\n\n"
	      "- PT: \n\n"
	      "- JT: \n\n"
	      "* %n\n\n"
	      "- CT: \n\n"
	      "- AT: \n\n\n"))


(define-key global-map (kbd "C-<f6>") 'org-wiki-insert-new)
(define-key global-map (kbd "C-<f7>") 'org-wiki-insert-link)
(define-key global-map (kbd "C-<f8>") 'org-open-at-point)




(provide 'init-org-wiki)
