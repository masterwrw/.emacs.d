;;;; jump
(require-maybe 'avy)
(defhydra+ hydra-jump (:exit t :idle 1.0)
  ("v" avy-goto-char-in-line "Jump inline")
  )



(provide 'init-avy)
