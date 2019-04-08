(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching
(setq ido-auto-merge-delay-time 10000) ;; disable auto search file
(eye--print-time "require ido")


(defhydra+ hydra-file (:exit t :idle 1.0)
  ("a" ido-switch-buffer "Switch buffer")
  ("o" ido-find-file "Find file"))



(provide 'init-ido)

