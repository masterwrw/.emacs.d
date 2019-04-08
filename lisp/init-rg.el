(require-maybe 'rg)

(defhydra+ hydra-search (:idle 1.0)
  ("g" rg "rg" :exit t))




(provide 'init-rg)
