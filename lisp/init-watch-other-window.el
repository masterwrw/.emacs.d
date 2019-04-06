(require 'watch-other-window)

(defhydra hydra-watch-other ()
  ("SPC" nil "quit")
  ("i" watch-other-window-down-line "Down line")
  ("k" watch-other-window-up-line "Up line")
  ("l" watch-other-window-down "Down scroll")
  ("j" watch-other-window-up "Up scroll"))


(defhydra+ hydra-window (:idle 1.0)
  ;; Don't show the new hydra view if no exit
  ("w" hydra-watch-other/body "Watch other" :exit t))



(provide 'init-watch-other-window)
