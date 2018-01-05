(require-package 'color-theme)

(require 'color-theme)
(color-theme-initialize)
;(color-theme-deep-blue)


;; dracula-theme, dark theme
(require-package 'dracula-theme)
;(load-theme 'dracula t)


;; leuven-theme, light and dark theme
(require-package 'leuven-theme)
;(load-theme 'leuven t) ; light
(require 'leuven-dark-theme)
(load-theme 'leuven-dark t)



(provide 'init-color-theme)
