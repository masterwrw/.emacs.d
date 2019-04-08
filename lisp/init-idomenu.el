;;;; imenu
(autoload 'idomenu "idomenu" "" t)

(defhydra+ hydra-imenu (:exit t :idle 1.0)
  ("i" idomenu))


(provide 'init-idomenu)
