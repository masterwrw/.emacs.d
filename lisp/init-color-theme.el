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
(if *is-windows*
    (load-theme 'solarized-light t)
  (load-theme 'solarized-dark t))




;; powerline, Rewrite of Powerline
(require-package 'powerline)
(require 'powerline)
;(powerline-default-theme)

;; air-themes, vim-airline themes for emacs powerline
(require-package 'airline-themes)
(require 'airline-themes)
(if *is-windows*
    (load-theme 'airline-solarized-gui t)
  (load-theme 'airline-molokai t))



(require-package 'nyan-mode)
(require 'nyan-mode)
(setq default-mode-line-format
      (list ""
            'mode-line-modified
            "<"
            "kirchhoff"
            "> "
            "%10b"
            '(:eval (when nyan-mode (list (nyan-create))));;注意添加此句到你的format配置列表中
            " "
            'default-directory
            " "
            "%[("
            'mode-name
            'minor-mode-list
            "%n"
            'mode-line-process
            ")%]--"
            "Line %l--"
            '(-3 . "%P")
            "-%-"))
(nyan-mode t)
(nyan-start-animation)



(provide 'init-color-theme)
