(eye--reset-time)
;;(require 'avy-zap)
(defhydra+ hydra-delete (:exit t :idle 1.0)
  ("c" avy-zap-to-char-dwim "Zap to char")
  ("v" avy-zap-up-to-char-dwim "Zap up to char"))

(eye--print-time "init-avy-zap")


(provide 'init-avy-zap)
