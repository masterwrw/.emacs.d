(require-maybe 'avy-zap)
(defhydra+ hydra-delete (:exit t :idle 1.0)
  ("c" avy-zap-to-char-dwim "Zap to char")
  ("v" avy-zap-up-to-char-dwim "Zap up to char"))



(provide 'init-avy-zap)
