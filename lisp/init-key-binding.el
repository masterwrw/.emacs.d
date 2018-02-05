;;; Basic key bindings
(defalias 'list-buffers 'ibuffer)

(define-key global-map "\ef" 'find-file)
(define-key global-map "\eF" 'find-file-other-window)

(global-set-key (read-kbd-macro "\eb") 'ivy-switch-buffer)
(global-set-key (read-kbd-macro "\eB") 'ivy-switch-buffer-other-window)

(define-key global-map "\et" 'load-todo)

;; no screwingwith middle mouse button
(global-unset-key [mouse-2])



;(define-key global-map [C-right] 'forward-word)
;(define-key global-map [C-left] 'backward-word)
(define-key global-map [C-up] 'previous-blank-line)
(define-key global-map [C-down] 'next-blank-line)
(define-key global-map [home] 'beginning-of-line)
(define-key global-map [end] 'end-of-line)
(define-key global-map [pgup] 'forward-page)
(define-key global-map [pgdown] 'backward-page)
(define-key global-map [C-next] 'scroll-other-window)
(define-key global-map [C-prior] 'scroll-other-window-down)


; ALT-alternatives
;(defadvice set-mark-command (after no-bloody-t-m-m activate)
;  "Prevent consecutive marks activating bloody `transient-mark-mode'."
;  (if transient-mark-mode (setq transient-mark-mode nil)))

;; if use this, mark region will no background
;(defadvice mouse-set-region-1 (after no-bloody-t-m-m activate)
;  "Prevent mouse commands activating bloody `transient-mark-mode'."
;  (if transient-mark-mode (setq transient-mark-mode nil)))

;(defun append-as-kill ()
;  "Performs copy-region-as-kill as an append."
;  (interactive)
;  (append-next-kill)
;  (copy-region-as-kill (mark) (point))
;  )
(define-key global-map "\e " 'set-mark-command)
(define-key global-map "\eq" 'copy-region-as-kill)
(define-key global-map "\ea" 'yank)
(define-key global-map "\ez" 'kill-region)
(define-key global-map [M-up] 'previous-blank-line)
(define-key global-map [M-down] 'next-blank-line)
(define-key global-map [M-right] 'forward-word)
(define-key global-map [M-left] 'backward-word)

(define-key global-map "\e:" 'View-back-to-mark)
(define-key global-map "\e;" 'exchange-point-and-mark)

(define-key global-map [f9] 'first-error)
(define-key global-map [f10] 'previous-error)
(define-key global-map [f11] 'next-error)

(define-key global-map "\en" 'next-error)
(define-key global-map "\eN" 'previous-error)

(define-key global-map "\eg" 'goto-line)
(define-key global-map "\ej" 'imenu)

(defun owen-replace-in-region (old-word new-word)
  "Perform a replace-string in the current region."
  (interactive "sReplace: \nsReplace: %s  With: ")
  (save-excursion (save-restriction
		    (narrow-to-region (mark) (point))
		    (beginning-of-buffer)
		    (replace-string old-word new-word)
		    ))
  )
(define-key global-map "\el" 'owen-replace-in-region)

(define-key global-map "\eo" 'query-replace)
(define-key global-map "\eO" 'owen-replace-string)

;; \377 is alt-backspace
(define-key global-map "\377" 'backward-kill-word)
(define-key global-map [M-delete] 'kill-word)

(define-key global-map "\e[" 'start-kbd-macro)
(define-key global-map "\e]" 'end-kbd-macro)
(define-key global-map "\e'" 'call-last-kbd-macro)

;; Buffers
(define-key global-map "\er" 'revert-buffer)
(define-key global-map "\ek" 'kill-this-buffer)
(define-key global-map "\es" 'save-buffer)


(global-set-key (kbd "S-<f1>")
  (lambda ()
    (interactive)
    (dired "~/")))

;;(add-hook 'racer-mode-hook #'company-mode)

(global-set-key (kbd "TAB") #'company-indent-or-complete-common) ;
(setq company-tooltip-align-annotations t)


(define-key global-map "\t" 'dabbrev-expand)
(define-key global-map [S-tab] 'indent-for-tab-command)
(define-key global-map [backtab] 'indent-for-tab-command)
(define-key global-map "\C-y" 'indent-for-tab-command)
(define-key global-map [C-tab] 'indent-region)
(define-key global-map "	" 'indent-region)

(define-key c++-mode-map [f12] 'owen-find-corresponding-file)
(define-key c++-mode-map [M-f12] 'owen-find-corresponding-file-other-window)

;; Alternate bindings for F-keyless setups (ie MacOS X terminal)
(define-key c++-mode-map "\ec" 'owen-find-corresponding-file)
(define-key c++-mode-map "\eC" 'owen-find-corresponding-file-other-window)


;; Newline indents, semi-colon doesn't
(define-key text-mode-map "\C-m" 'newline-and-indent)

;; Prevent overriding of alt-s
(define-key text-mode-map "\es" 'owen-save-buffer)
(define-key c++-mode-map "\es" 'owen-save-buffer)

(define-key c++-mode-map "\t" 'dabbrev-expand)
(define-key c++-mode-map [S-tab] 'indent-for-tab-command)
(define-key c++-mode-map "\C-y" 'indent-for-tab-command)
(define-key c++-mode-map [C-tab] 'indent-region)
(define-key c++-mode-map "	" 'indent-region)

(define-key c++-mode-map "\ej" 'imenu)

(define-key c++-mode-map "\e." 'c-fill-paragraph)

(define-key c++-mode-map "\e/" 'c-mark-function)

(define-key c++-mode-map "\e " 'set-mark-command)
(define-key c++-mode-map "\eq" 'copy-region-as-kill)
(define-key c++-mode-map "\ea" 'yank)
(define-key c++-mode-map "\ez" 'kill-region)
(define-key c++-mode-map "\C-m" 'newline-and-indent)


(bind-key "M-x" 'counsel-M-x)

(define-key global-map [C-left] 'backward-forward-previous-location)
(define-key global-map [C-right] 'backward-forward-next-location)

(define-key global-map "\ep" 'quick-calc)
(define-key global-map "\ew" 'other-window)
(define-key compilation-mode-map (kbd "M-w") 'other-window)

(bind-key "M-<down>" 'my-fast-step-downward)
(bind-key "M-<up>" 'my-fast-step-upward)
(bind-key "M-<right>" 'forward-word)
(bind-key "M-<left>" 'backward-word)

(define-key org-mode-map (kbd "M-<down>") 'my-fast-step-downward)
(define-key org-mode-map (kbd "M-<up>") 'my-fast-step-upward)

(define-key global-map (kbd "M-/") 'xah-comment-dwim)

(bind-key "C-<f1>" 'my-auto-switch-buffer)
(bind-key "C-<f2>" 'swiper)
(bind-key "C-<f3>" 'delete-other-windows)

(if *is-windows*
    (bind-key "C-<wheel-up>" 'text-scale-increase)
    (bind-key "C-<mouse-4>" 'text-scale-increase))

(if *is-windows*
    (bind-key "C-<wheel-down>" 'text-scale-decrease)
    (bind-key "C-<mouse-5>" 'text-scale-decrease))

(bind-key "C-=" 'cnfonts-increase-fontsize)
(bind-key "C--" 'cnfonts-decrease-fontsize)

(if *is-windows*
    (bind-key "<wheel-up>" 'scroll-down-lines)
    (bind-key "<mouse-4>" 'scroll-down-lines))
(if *is-windows*
    (bind-key "<wheel-down>" 'scroll-up-lines)
    (bind-key "<mouse-5>" 'scroll-up-lines))


(provide 'init-key-binding)
