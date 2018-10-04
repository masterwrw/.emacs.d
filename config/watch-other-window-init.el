(require 'watch-other-window)

(ryo-modal-keys
 ("C-/ "
  (("i" watch-other-window-down-line)
   ("k" watch-other-window-up-line)
   ("j" watch-other-window-down)
   ("l" watch-other-window-up))))


(provide 'watch-other-window-init)
