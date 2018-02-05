;;; Color theme configuration

(global-hl-line-mode 1)
(set-face-background 'hl-line "midnight blue")
(setq owen-font "outline-DejaVu Sans Mono")

(when *is-windows*
  (setq owen-font "outline-Liberation Mono")
  )

(when *is-linux*
  (display-battery-mode)
  )

;; color-identifiers-mode, Color identifiers based on their names
(require-package 'color-identifiers-mode)
(add-hook 'after-init-hook 'global-color-identifiers-mode)

(load-library "view")

(add-to-list 'default-frame-alist '(font . "Liberation Mono-11.5"))
(set-face-attribute 'default t :font "Liberation Mono-11.5")
(set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
(set-face-attribute 'font-lock-comment-face nil :foreground "gray50")
(set-face-attribute 'font-lock-constant-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-doc-face nil :foreground "gray50")
(set-face-attribute 'font-lock-function-name-face nil :foreground "burlywood3")
(set-face-attribute 'font-lock-keyword-face nil :foreground "DarkGoldenrod3")
(set-face-attribute 'font-lock-string-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-type-face nil :foreground "burlywood3")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "burlywood3")


(defun post-load-stuff ()
  (interactive)
  (maximize-frame)
  (set-foreground-color "burlywood3")
  (set-background-color "#161616")
  (set-face-background 'region "blue")
  (set-face-foreground 'region "gray50")
  (set-cursor-color "#40FF40")
  )
(add-hook 'window-setup-hook 'post-load-stuff t)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)


;(require-package 'color-theme)

;(require 'color-theme)
;(color-theme-initialize)
;(color-theme-deep-blue)


;; dracula-theme, dark theme
(require-package 'dracula-theme)
(load-theme 'dracula t)


;; atom-one-dark-theme
(require-package 'atom-one-dark-theme)
(require 'atom-one-dark-theme)
;(load-theme 'atom-one-dark t)


;; hemisu-theme
(require-package 'hemisu-theme)
(require 'hemisu-theme)


;; leuven-theme, light and dark theme
(require-package 'leuven-theme)
;(load-theme 'leuven t) ; light
;(require 'leuven-dark-theme)
;(load-theme 'leuven-dark t)


(require-package 'solarized-theme)
(require 'solarized-theme)


(require-package 'material-theme)
;(load-theme 'material t)


;;================== mode line theme ===================
;; powerline, Rewrite of Powerline
(require-package 'powerline)
(require 'powerline)
;(powerline-center-theme)

;; air-themes, vim-airline themes for emacs powerline
(require-package 'airline-themes)
(require 'airline-themes)
;(load-theme 'airline-molokai t)




(provide 'init-color-theme)
