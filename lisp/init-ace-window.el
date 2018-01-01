(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (bind-key "C-o" 'ace-window)
    ))

;; Hoding the shift key and with the arrows, go to other window
(windmove-default-keybindings)

(provide 'init-ace-window)
