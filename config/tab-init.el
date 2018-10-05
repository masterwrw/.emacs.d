(require 'awesome-tab)

(awesome-tab-mode t)

(set-face-attribute 'awesome-tab-selected nil :foreground "cyan3")
(set-face-attribute 'awesome-tab-unselected nil :foreground "cyan4")

(ryo-modal-keys
 ("ss" mode-line-other-buffer)  ;;快速切换两个buffer
 ("sg" awesome-tab-switch-group)
 ("sj" awesome-tab-backward)
 ("sl" awesome-tab-forward)
 ("sh" awesome-tab-select-beg-tab)
 ("s;" awesome-tab-select-end-tab)
 ("sk" awesome-tab-forward-group)
 ("si" awesome-tab-backward-group)
 ("C-." 
  (("sj" awesome-tab-move-current-tab-to-left)
   ("sl" awesome-tab-move-current-tab-to-right)
   ("sk" awesome-tab-kill-all-buffers-in-current-group)
   ("so" awesome-tab-kill-other-buffers-in-current-group)))
 )


(provide 'tab-init)
