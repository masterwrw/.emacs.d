(add-to-list 'load-path "~/packages/dash")
(add-to-list 'load-path "~/packages/solarized-emacs")
(require-maybe 'solarized)
;; make the modeline high contrast
(setq solarized-high-contrast-mode-line t)
;; Use less bolding
(setq solarized-use-less-bold t)
;; Use more italics
(setq solarized-use-more-italic t)

(load-theme 'solarized-dark t)


;; (add-to-list 'load-path "~/packages/spolsky-theme")
;; (require 'spolsky-theme)
;; (load-theme 'spolsky t)




(provide 'init-theme)
