;; fixed minibuffe
;(setq resize-mini-windows nil)
;(setq ivy-height 6)

;(defun eye/set-mini-window-height (&optional frame)
;  (interactive)
;  (let ((mini-win (minibuffer-window frame)))
;	(when (and mini-win (< (window-size mini-win) ivy-height))
;	  (window-resize mini-win ivy-height))))

;; (add-hook 'window-setup-hook 'eye/set-mini-window-height)
;; (add-hook 'after-make-frame-functions 'eye/set-mini-window-height)
;; (add-hook 'move-frame-functions 'eye/set-mini-window-height)
;; no need above hook if use code below
;(defun eye/mini-ivy-on ()
;  (interactive)
;  (add-hook 'window-size-change-functions 'eye/set-mini-window-height)
;  (add-hook 'after-init-hook 'eye/set-mini-window-height))
;(defun eye/mini-ivy-off ()
;  (interactive)
;  (remove-hook 'window-size-change-functions 'eye/set-mini-window-height)
;  (remove-hook 'after-init-hook 'eye/set-mini-window-height))


(provide 'init-minibuffer)
