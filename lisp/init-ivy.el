(eye--reset-time)

(require 'ivy)
(require 'counsel)
(require 'swiper)

(defhydra+ hydra-file (:exit t :idle 1.0)
  ("a" counsel-ibuffer)
  ("g" counsel-git "git find");查找在git仓库中的文件，注意最好子目录下没有.git目录，否则可能不会显示出文件列表
  ("h" counsel-recentf "recnetf")
  ("c" counsel-find-file))


(defhydra+ hydra-search (:idle 1.0)
  ("s" swiper "swiper" :exit t)
  ("l" counsel-rg "rg" :exit t)
  ("k" counsel-rg-marked "rg marked" :exit t))


(defhydra+ hydra-imenu (:exit t :idle 1.0)
  ("c" counsel-semantic-or-imenu "counsel imenu"))

(eye--print-time "init-ivy")



(provide 'init-ivy)
