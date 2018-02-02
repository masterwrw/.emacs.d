;; avy, Jump to arbitrary positions in visible text and select text quickly.
(use-package avy
  :ensure t)


;; ace-window, Quickly switch windows.
(require-package 'ace-jump-mode)
(use-package ace-window
  :ensure t)


;; Hoding the shift key and with the arrows, go to other window
;(windmove-default-keybindings)


;; goto-chg, Goto Last Change
(require-package 'goto-chg)
(require 'goto-chg)



(provide 'init-navigation)
