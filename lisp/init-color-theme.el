(require-package 'color-theme)

(require 'color-theme)
(color-theme-initialize)
;(color-theme-deep-blue)


;; dracula-theme, dark theme
;(require-package 'dracula-theme)
;(load-theme 'dracula t)


;; leuven-theme, light and dark theme
(require-package 'leuven-theme)
(load-theme 'leuven t) ; light
;(require 'leuven-dark-theme)
;(load-theme 'leuven-dark t)


(require-package 'solarized-theme)
(require 'solarized-theme)



;; powerline, Rewrite of Powerline
(require-package 'powerline)
(require 'powerline)
(powerline-center-theme)

;; air-themes, vim-airline themes for emacs powerline
(require-package 'airline-themes)
(require 'airline-themes)
(load-theme 'airline-molokai t))




(provide 'init-color-theme)
