(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;; Set emacs title
; https://stackoverflow.com/questions/3669511/the-function-to-show-current-files-full-path-in-mini-buffer#
;(setq frame-title-format
;      (list (format "%s %%S: %%j " (system-name))
;            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(setq frame-title-format "-- master -- %f -- %b")

;; No tool bar
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

;; No scroll bar
(if (fboundp 'set-scroll-bar-mode)
    (set-scroll-bar-mode nil))

;; No menu bar
;(menu-bar-mode -1)

;; No fringe, http://emacsredux.com/blog/2015/01/18/customizing-the-fringes/
;(set-window-fringes nil 0 0)
;(fringe-mode '(0 . 0))

;; No ring bell audio, http://emacsredux.com/blog/2016/02/14/disable-annoying-audio-notifications/
(setq ring-bell-function 'ignore)

;; Window Commands
(defun w32-restore-frame ()
  "Restore a minimized frame"
  (interactive)
  (w32-send-sys-command 61728))

;; Maximizing on startup
(defun maximize-frame ()
  "Maximizes the active frame in Windows"
  (interactive)
  ;; Send a `WM_SYSCOMMAND' message to the active frame with the
  ;; `SC_MAXIMIZE' parameter.
  (when (eq system-type 'windows-nt)
    (w32-send-sys-command 61488)))



(provide 'init-gui)
