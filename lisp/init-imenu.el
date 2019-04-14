(require-maybe 'imenu)

(setq imenu-auto-rescan t)
(setq imenu-auto-rescan-maxout 500000)

(defhydra hydra-imenu (:exit t :idle 1.0)
  ("i" imenu))


(provide 'init-imenu)
