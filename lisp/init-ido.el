(require-package 'flx-ido)
(require-package 'ido-vertical-mode)

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(require 'ido-vertical-mode)
;; (ido-mode 1)
(ido-vertical-mode 1)
;; (setq ido-vertical-define-keys 'C-n-and-C-p-only)


(provide 'init-ido)
