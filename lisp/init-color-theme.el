(require-package 'color-theme)

(require 'color-theme)
(color-theme-initialize)
;(color-theme-deep-blue)


;; dracula-theme, dark theme
;(require-package 'dracula-theme)
;(load-theme 'dracula t)


;; leuven-theme, light and dark theme
(require-package 'leuven-theme)
;(load-theme 'leuven t) ; light
;(require 'leuven-dark-theme)
;(load-theme 'leuven-dark t)


(require-package 'solarized-theme)
(require 'solarized-theme)
(if *win64*
    (load-theme 'solarized-light)
  (load-theme 'solarized-dark))




;; powerline, Rewrite of Powerline
(require-package 'powerline)
(require 'powerline)
;(powerline-default-theme)

;; air-themes, vim-airline themes for emacs powerline
(require-package 'airline-themes)
(require 'airline-themes)
(if *win64*
    (load-theme 'airline-solarized-gui)
  (load-theme 'airline-molokai))







(provide 'init-color-theme)
