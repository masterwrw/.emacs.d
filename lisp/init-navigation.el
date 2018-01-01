;; avy, Jump to arbitrary positions in visible text and select text quickly.
(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-char))


;; ace-window, Quickly switch windows.
(require-package 'ace-jump-mode)
(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (bind-key "C-o" 'ace-window)
    ))


;; Hoding the shift key and with the arrows, go to other window
(windmove-default-keybindings)




(provide 'init-navigation)
