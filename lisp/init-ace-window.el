(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (bind-key "C-o" 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :heaght 3.0)))))
    ))

;; Hoding the shift key and with the arrows, go to other window
(windmove-default-keybindings)

(provide 'init-ace-window)
