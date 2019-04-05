(eye--reset-time)

(require 'rg)


(defhydra+ hydra-search (:idle 1.0)
  ("g" rg "rg" :exit t))


(eye--print-time "init-rg")




(provide 'init-rg)
