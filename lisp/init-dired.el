(require 'dired)

(defhydra+ hydra-file (:exit t :idle 1.0)
  ("d" dired-jump "Dired"))



(provide 'init-dired)
