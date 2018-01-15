(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;; Set emacs title
; https://stackoverflow.com/questions/3669511/the-function-to-show-current-files-full-path-in-mini-buffer#
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))


;; No tool bar
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

;; No scroll bar
(if (fboundp 'set-scroll-bar-mode)
    (set-scroll-bar-mode nil))

(if (not *is-windows*)
    (progn
      (if (fboundp 'menu-bar-mode) ;; No menu bar
          (menu-bar-mode -1)))
  (progn
    (require-package 'start-menu)
    (require 'start-menu)
    (start-menu-enable)))



(provide 'init-gui)
