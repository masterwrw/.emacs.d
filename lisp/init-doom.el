;;;; all-the-icons
(require 'all-the-icons)
(unless (or is-windows (member "all-the-icons" (font-family-list)))
  (all-the-icons-install-fonts t))

(add-to-list 'all-the-icons-icon-alist
             '("\\.ipynb" all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
(add-to-list 'all-the-icons-mode-icon-alist
             '(ein:notebooklist-mode all-the-icons-faicon "book" :face all-the-icons-orange))
(add-to-list 'all-the-icons-mode-icon-alist
             '(ein:notebook-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
(add-to-list 'all-the-icons-mode-icon-alist
             '(ein:notebook-multilang-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
(add-to-list 'all-the-icons-icon-alist
             '("\\.epub$" all-the-icons-faicon "book" :face all-the-icons-green))
(add-to-list 'all-the-icons-mode-icon-alist
             '(nov-mode all-the-icons-faicon "book" :face all-the-icons-green))
(add-to-list 'all-the-icons-mode-icon-alist
             '(gfm-mode  all-the-icons-octicon "markdown" :face all-the-icons-lblue))

;;;; solaire-mode
(require 'solaire-mode)

;; Enable solaire-mode anywhere it can be enabled
(solaire-global-mode +1)
;; To enable solaire-mode unconditionally for certain modes:
(add-hook 'ediff-prepare-buffer-hook #'solaire-mode)

;; ...if you use auto-revert-mode, this prevents solaire-mode from turning
;; itself off every time Emacs reverts the file
(add-hook 'after-revert-hook #'turn-on-solaire-mode)

;; highlight the minibuffer when it is activated:
(add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)

;; if the bright and dark background colors are the wrong way around, use this
;; to switch the backgrounds of the `default` and `solaire-default-face` faces.
;; This should be used *after* you load the active theme!
;;
;; NOTE: This is necessary for themes in the doom-themes package!
(solaire-mode-swap-bg)


;;;; doom theme
(require 'doom-themes)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)
;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

(load-theme 'doom-Iosvkem t)

;;;; doom modeline
(require 'doom-modeline)
(setq doom-modeline-major-mode-color-icon t)
(setq doom-modeline-github t)

;; How tall the mode-line should be (only respected in GUI Emacs).
(setq doom-modeline-height 35)
;; How wide the mode-line bar should be (only respected in GUI Emacs).
(setq doom-modeline-bar-width 3)

;; don't use github notifications,
;; because must set github.user, if not, will has a lot of emacs error process
(setq doom-modeline-github nil)
(with-eval-after-load 'doom-modeline-mode
  (cancel-timer doom-modeline--github-timer))

;; (doom-modeline-mode 1)

;;;; time
(require 'time)
(setq display-time-24hr-format t)
(setq display-time-day-and-date nil)
(display-time-mode)




(provide 'init-doom)
