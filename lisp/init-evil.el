;;; evil package

(require-package 'evil)
(require 'evil)
(evil-mode 1)


;; Do not use keymap on insert mode of evil
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map (read-kbd-macro evil-toggle-key) 'evil-normal-state)
(define-key evil-insert-state-map [escape] 'evil-normal-state)



(provide 'init-evil)
