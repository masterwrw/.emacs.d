(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;; Set emacs title
(setq frame-title-format "%b")

;; No tool bar
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

;; No scroll bar
(if (fboundp 'set-scroll-bar-mode)
    (set-scroll-bar-mode nil))

;; No menu bar
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))


;; powerline, Rewrite of Powerline
(require-package 'powerline)
(require 'powerline)
(powerline-default-theme)

;; air-themes, vim-airline themes for emacs powerline
(require-package 'airline-themes)
(require 'airline-themes)
(load-theme 'airline-solarized-gui)




(provide 'init-gui)
